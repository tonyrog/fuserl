#ifndef __FUSERL_PORTABILITY_H_
#define __FUSERL_PORTABILITY_H_

int
fuserl_errno_decanonicalize             (int canonical);

int
fuserl_access_mode_canonicalize         (int mode);

int
fuserl_attr_flags_canonicalize          (int flags);

int
fuserl_lock_cmd_canonicalize            (int cmd);

short
fuserl_l_type_canonicalize              (short l_type);

short
fuserl_l_whence_canonicalize            (short l_whence);

int
fuserl_open_flags_canonicalize          (int flags);

int
fuserl_open_flags_decanonicalize        (int flags);

short
fuserl_l_type_decanonicalize            (short l_type);

short
fuserl_l_whence_decanonicalize          (short l_whence);

int
fuserl_stat_mode_canonicalize           (mode_t mode);

mode_t
fuserl_stat_mode_decanonicalize         (int canonical);

#endif /* __FUSERL_PORTABILITY_H_ */
