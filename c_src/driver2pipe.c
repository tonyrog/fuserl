/* I don't understand why these two functions need to redirected 
 * and no others ... 
 */
#define driver_failure_posix driver_failure_posix_flass
#define driver_failure_eof driver_failure_eof_flass
#include <erl_driver.h>
#undef driver_failure_posix
#undef driver_failure_eof

#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include "driver2pipe.h"

/*=====================================================================*
 *                              Packet IO                              *
 *=====================================================================*/

static size_t
io_exact (int           fd,
          char*         buf,
          size_t        len,
          ssize_t     (*f) (int, void *, size_t))
{
  size_t got = 0;

  while (got < len)
    {
      int r;

      if ((r = f (fd, buf + got, len - got)) <= 0)
        {
          if (r == 0 || errno != EINTR)
            {
              return got;
            }
        }
      else
        {
          got += r;
        }
    }

  return got;
}

static size_t
read_exact (int         fd, 
            char       *buf,
            size_t      len)
{
  return io_exact (fd, buf, len, read);
}

static int
read_packet (int                fd,
             unsigned int       header_bytes,
             char*              buf,
             size_t             buf_size)
{
  char header[header_bytes];
  unsigned char *tmp;
  size_t len = 0;

  if (read_exact (fd, header, header_bytes) != header_bytes)
    {
      return -1;
    }

  for (tmp = (unsigned char *) header; header_bytes > 0; --header_bytes, ++tmp)
    {
      len <<= 8;
      len |= *tmp;
    }

  if (len > buf_size) 
    {
      fprintf (stderr, 
               "driver2pipe: read_packet: packet too large: %lx\n",
               (long) len);

      return -1;
    }

  return (read_exact (fd, buf, len) == len) ? (ssize_t) len : -1;
}

static size_t
write_exact (int        fd, 
             char*      buf,
             size_t     len)
{
  return io_exact (fd, buf, len, (ssize_t (*) (int, void *, size_t)) write);
}

static int
write_packet (int               fd,
              unsigned int      header_bytes,
              char*             buf,
              size_t            len)
{
  char header[header_bytes];
  char *tmp;
  unsigned int tmplen;

  for (tmp = header + header_bytes, tmplen = len; 
       tmp > header;
       tmplen >>= 8)
    {
      *--tmp = tmplen & 0xff;
    }

  if (write_exact (fd, header, header_bytes) != header_bytes)
    {
      return -1;
    }

  return write_exact (fd, buf, len);
}

/*=====================================================================*
 *                Erlang driver function implementations               *
 *=====================================================================*/

typedef struct _PollInfo PollInfo;
struct _PollInfo 
{
  struct pollfd*        fds;
  nfds_t                nfds;
};

void *
driver_alloc (size_t s)
{
  return malloc (s);
}

void *
driver_realloc (void* ptr, 
                size_t size)
{
  return realloc (ptr, size);
}

void
driver_free (void* p)
{
  free (p);
}

int 
driver_select (ErlDrvPort       port,
               ErlDrvEvent      event,
               int              mode,
               int              on)
{
  PollInfo* poll_info = (PollInfo*) port;
  nfds_t i;

  for (i = 0; i < poll_info->nfds; ++i)
    {
      if (poll_info->fds[i].fd == (intptr_t) event)
        {
          if (on)
            {
              poll_info->fds[i].events = 0;
              poll_info->fds[i].events |= (mode & DO_READ) ? POLLIN : 0;
              poll_info->fds[i].events |= (mode & DO_WRITE) ? POLLOUT : 0;
            }
          else /* ! on */
            {
              if (i + 1 < poll_info->nfds)
                {
                  poll_info->fds[i] = poll_info->fds[poll_info->nfds - 1];
                }

              --poll_info->nfds;
            }

          return 0;
        }
    }

  if (on)
    {
      poll_info->fds = 
        driver_realloc (poll_info->fds,
                        (poll_info->nfds + 1) * sizeof (struct pollfd));

      poll_info->fds[poll_info->nfds].fd = (intptr_t) event;
      poll_info->fds[poll_info->nfds].events = 0;
      poll_info->fds[poll_info->nfds].events |= (mode & DO_READ) ? POLLIN : 0;
      poll_info->fds[poll_info->nfds].events |= (mode & DO_WRITE) ? POLLOUT : 0;
      poll_info->fds[poll_info->nfds].revents = 0;

      ++poll_info->nfds;
    }

  return 0;
}

int 
driver_output (ErlDrvPort       port,
               char*            buf,
               ErlDrvSizeT      len)
{
  (void) port;

  return write_packet (4, 4, buf, len);
}

int
driver_failure_posix (ErlDrvPort        port,
                      int               errno);

int
driver_failure_posix (ErlDrvPort        port,
                      int               errno)
{
  PollInfo* poll_info = (PollInfo*) port;

  (void) errno;

  close (poll_info->fds[0].fd);

  return 0;
}

int
driver_failure_eof (ErlDrvPort        port);

int
driver_failure_eof (ErlDrvPort        port)
{
  PollInfo* poll_info = (PollInfo*) port;

  (void) errno;

  close (poll_info->fds[0].fd);

  return 0;
}

static volatile int signalled = 0;

static void
handle_signal (int sig)
{
  (void) sig;

  signalled = 1;
}

static void
setup_signal_handlers (void)
{
  sigset_t blocked;
  struct sigaction act;

  memset (&act, 0, sizeof (struct sigaction));
  act.sa_handler = handle_signal;
  sigfillset (&act.sa_mask);

  sigaction (SIGHUP, &act, NULL);
  sigaction (SIGTERM, &act, NULL);
  sigaction (SIGINT, &act, NULL);

  sigfillset (&blocked);
  sigdelset (&blocked, SIGHUP);
  sigdelset (&blocked, SIGTERM);
  sigdelset (&blocked, SIGINT);
  sigdelset (&blocked, SIGILL);
  sigdelset (&blocked, SIGSEGV);
  sigdelset (&blocked, SIGBUS);
  sigdelset (&blocked, SIGCONT);
  sigdelset (&blocked, SIGSTOP);

  sigprocmask (SIG_SETMASK, &blocked, NULL);
}

int 
driver_to_pipe_main (char*              command,
                     ErlDrvEntry*       entry)
{
  ErlDrvData d;
  PollInfo info;
  char buf[1 << 20];

  setup_signal_handlers ();

  info.fds = driver_alloc (sizeof (struct pollfd));
  info.fds[0].fd = 3;
  info.fds[0].events = POLLIN;
  info.fds[0].revents = 0;
  info.nfds = 1;

  if (entry->init) { entry->init (); }
  d = entry->start ((void *) &info, command);

  while (! signalled)
    {
      nfds_t i;
      nfds_t nfds;

      nfds = info.nfds;

      for (i = 0; i < nfds; ++i)
        {
          info.fds[i].revents = 0;
        }

      if (poll (info.fds, nfds, 1000) < 0)
        {
          if (errno != EINTR)
            {
              const char msg[] = "driver2pipe: bad poll\n";

              /* XXX: Compiler hangs on warnings,
               *       including not dealing with a return value. (Is there a better way to temporarily disable -Werror=unused-result ?)
               */
              if(write (2, msg, sizeof (msg))){
              }

              goto STOP;
            }
        }

      if (info.fds[0].revents)
        {
          ssize_t size;

          size = read_packet (3, 4, buf, sizeof (buf));

          if (size < 0)
            {
              goto STOP;
            }

          entry->output (d, buf, size);
        }

      for (i = 1; i < nfds; ++i)
        {
          if (info.fds[i].revents & POLLNVAL)
            {
              fprintf (stderr, 
                       "fatal: poll got nval (%d) for file descriptor %d "
                       "wanted: %d\n",
                       info.fds[i].revents,
                       info.fds[i].fd,
                       info.fds[i].events);

              goto STOP;
            }

          if (info.fds[i].revents & POLLERR)
            {
              fprintf (stderr, 
                       "fatal: poll got err (%d) for file descriptor %d "
                       "wanted: %d\n",
                       info.fds[i].revents,
                       info.fds[i].fd,
                       info.fds[i].events);

              goto STOP;
            }

          if (info.fds[i].revents & POLLIN)
            {
              entry->ready_input (d, (ErlDrvEvent) (intptr_t) info.fds[i].fd);
            }

          if (info.fds[i].revents & POLLOUT)
            {
              entry->ready_output (d, (ErlDrvEvent) (intptr_t) info.fds[i].fd);
            }
        }
    }

STOP:
  entry->stop (d);

  if (entry->finish) { entry->finish (); }

  driver_free (info.fds);

  return signalled;
}
