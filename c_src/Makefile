#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=fuserl
CFLAGS+= -Wall -O3 -D_FILE_OFFSET_BITS=64 -DFUSE_USE_VERSION=26 -DHAVE_SETXATTR=1
LDFLAGS+=-shared

SO_OBJS = fuserl.o fuserl_portability.o
NIF_OBJS = fuserl_nif.o fuserl_portability.o
OBJS = fuserldrv.o driver2pipe.o
LIBS = -lfuse

all: ../priv/$(APP).so ../priv/$(APP)drv ../priv/$(APP)_nif.so

clean:
	rm -f ../priv/$(APP).so ../priv/$(APP)drv $(SO_OBJS) $(OBJS)

../priv/$(APP).so: $(SO_OBJS)
	$(CC) $(LDFLAGS) $(SO_OBJS) $(LIBS) -o$@

../priv/$(APP)drv: $(OBJS)
	$(CC) $(OBJS) $(LIBS) -o$@

../priv/$(APP)_nif.so: $(NIF_OBJS)
	$(CC) $(LDFLAGS) $(NIF_OBJS) $(LIBS) -o$@

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
