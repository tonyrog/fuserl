%% @doc Some functions to help portability.
%% @hidden
%% @end

-module (fuserlportable).
-export ([ canonicalize_errno/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

% returned by file: methods, supported for convenience
canonicalize_errno (badarg) -> canonicalize_errno (einval);
canonicalize_errno (terminated) -> canonicalize_errno (eio);
canonicalize_errno (system_limit) -> canonicalize_errno (emfile);

% the posix () set of atoms
canonicalize_errno (e2big) -> 0;
canonicalize_errno (eacces) -> 1;
canonicalize_errno (eaddrinuse) -> 2;
canonicalize_errno (eaddrnotavail) -> 3;
canonicalize_errno (eadv) -> 4;
canonicalize_errno (eafnosupport) -> 5;
canonicalize_errno (eagain) -> 6;
canonicalize_errno (ealign) -> 7;
canonicalize_errno (ealready) -> 8;
canonicalize_errno (ebade) -> 9;
canonicalize_errno (ebadf) -> 10;
canonicalize_errno (ebadfd) -> 11;
canonicalize_errno (ebadmsg) -> 12;
canonicalize_errno (ebadr) -> 13;
canonicalize_errno (ebadrpc) -> 14;
canonicalize_errno (ebadrqc) -> 15;
canonicalize_errno (ebadslt) -> 16;
canonicalize_errno (ebfont) -> 17;
canonicalize_errno (ebusy) -> 18;
canonicalize_errno (echild) -> 19;
canonicalize_errno (echrng) -> 20;
canonicalize_errno (ecomm) -> 21;
canonicalize_errno (econnaborted) -> 22;
canonicalize_errno (econnrefused) -> 23;
canonicalize_errno (econnreset) -> 24;
canonicalize_errno (edeadlk) -> 25;
canonicalize_errno (edeadlock) -> 26;
canonicalize_errno (edestaddrreq) -> 27;
canonicalize_errno (edirty) -> 28;
canonicalize_errno (edom) -> 29;
canonicalize_errno (edotdot) -> 30;
canonicalize_errno (edquot) -> 31;
canonicalize_errno (eduppkg) -> 32;
canonicalize_errno (eexist) -> 33;
canonicalize_errno (efault) -> 34;
canonicalize_errno (efbig) -> 35;
canonicalize_errno (ehostdown) -> 36;
canonicalize_errno (ehostunreach) -> 37;
canonicalize_errno (eidrm) -> 38;
canonicalize_errno (einit) -> 39;
canonicalize_errno (einprogress) -> 40;
canonicalize_errno (eintr) -> 41;
canonicalize_errno (einval) -> 42;
canonicalize_errno (eio) -> 43;
canonicalize_errno (eisconn) -> 44;
canonicalize_errno (eisdir) -> 45;
canonicalize_errno (eisnam) -> 46;
canonicalize_errno (elbin) -> 47;
canonicalize_errno (el2hlt) -> 48;
canonicalize_errno (el2nsync) -> 49;
canonicalize_errno (el3hlt) -> 50;
canonicalize_errno (el3rst) -> 51;
canonicalize_errno (elibacc) -> 52;
canonicalize_errno (elibbad) -> 53;
canonicalize_errno (elibexec) -> 54;
canonicalize_errno (elibmax) -> 56;
canonicalize_errno (elibscn) -> 57;
canonicalize_errno (elnrng) -> 58;
canonicalize_errno (eloop) -> 59;
canonicalize_errno (emfile) -> 60;
canonicalize_errno (emlink) -> 61;
canonicalize_errno (emsgsize) -> 62;
canonicalize_errno (emultihop) -> 63;
canonicalize_errno (enametoolong) -> 64;
canonicalize_errno (enavail) -> 65;
canonicalize_errno (enet) -> 66;
canonicalize_errno (enetdown) -> 67;
canonicalize_errno (enetreset) -> 68;
canonicalize_errno (enetunreach) -> 69;
canonicalize_errno (enfile) -> 70;
canonicalize_errno (enoano) -> 71;
canonicalize_errno (enobufs) -> 72;
canonicalize_errno (enocsi) -> 73;
canonicalize_errno (enodata) -> 74;
canonicalize_errno (enodev) -> 75;
canonicalize_errno (enoent) -> 76;
canonicalize_errno (enoexec) -> 77;
canonicalize_errno (enolck) -> 78;
canonicalize_errno (enolink) -> 79;
canonicalize_errno (enomem) -> 80;
canonicalize_errno (enomsg) -> 81;
canonicalize_errno (enonet) -> 82;
canonicalize_errno (enopkg) -> 83;
canonicalize_errno (enoprotoopt) -> 84;
canonicalize_errno (enospc) -> 85;
canonicalize_errno (enosr) -> 86;
canonicalize_errno (enosym) -> 87;
canonicalize_errno (enosys) -> 88;
canonicalize_errno (enotblk) -> 89;
canonicalize_errno (enotconn) -> 90;
canonicalize_errno (enotdir) -> 91;
canonicalize_errno (enotempty) -> 92;
canonicalize_errno (enotnam) -> 93;
canonicalize_errno (enotsock) -> 94;
canonicalize_errno (enotsup) -> 95;
canonicalize_errno (enotty) -> 96;
canonicalize_errno (enotuniq) -> 97;
canonicalize_errno (enxio) -> 98;
canonicalize_errno (eopnotsupp) -> 99;
canonicalize_errno (eperm) -> 100;
canonicalize_errno (epfnosupport) -> 101;
canonicalize_errno (epipe) -> 102;
canonicalize_errno (eproclim) -> 103;
canonicalize_errno (eprocunavail) -> 104;
canonicalize_errno (eprogmismatch) -> 105;
canonicalize_errno (eprogunavail) -> 106;
canonicalize_errno (eproto) -> 107;
canonicalize_errno (eprotonosupport) -> 108;
canonicalize_errno (eprototype) -> 109;
canonicalize_errno (erange) -> 110;
canonicalize_errno (erefused) -> 111;
canonicalize_errno (eremchg) -> 112;
canonicalize_errno (eremdev) -> 113;
canonicalize_errno (eremote) -> 114;
canonicalize_errno (eremoteio) -> 115;
canonicalize_errno (eremoterelease) -> 116;
canonicalize_errno (erofs) -> 117;
canonicalize_errno (erpcmismatch) -> 118;
canonicalize_errno (erremote) -> 119;
canonicalize_errno (eshutdown) -> 120;
canonicalize_errno (esocktnosupport) -> 121;
canonicalize_errno (espipe) -> 122;
canonicalize_errno (esrch) -> 123;
canonicalize_errno (esrmnt) -> 124;
canonicalize_errno (estale) -> 125;
canonicalize_errno (esuccess) -> 126;
canonicalize_errno (etime) -> 127;
canonicalize_errno (etimedout) -> 128;
canonicalize_errno (etoomanyrefs) -> 129;
canonicalize_errno (etxtbsy) -> 130;
canonicalize_errno (euclean) -> 131;
canonicalize_errno (eunatch) -> 132;
canonicalize_errno (eusers) -> 133;
canonicalize_errno (eversion) -> 134;
canonicalize_errno (ewouldblock) -> 135;
canonicalize_errno (exdev) -> 136;
canonicalize_errno (exfull) -> 137;
canonicalize_errno (nxdomain) -> 138;

% "no error"
canonicalize_errno (ok) -> canonicalize_errno (none);
canonicalize_errno (success) -> canonicalize_errno (none);
canonicalize_errno (none) -> 139.
