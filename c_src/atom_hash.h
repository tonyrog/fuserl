#ifndef __ATOM_HASH_H__
#define __ATOM_HASH_H__

#include "erl_nif.h"
#include "debug.h"

typedef struct _helem_t {
    struct _helem_t* next;  // next in chain    
    ERL_NIF_TERM* atm_ptr;  // the hash atom
    unsigned int  enm;      // enumerated value
    unsigned int  hval;     // hash value
} helem_t;

#define HELEM(a, e) { .next = NULL, .atm_ptr = &(a), .enm = (e), .hval = 0}

static unsigned int hash_atom(ERL_NIF_TERM term)
{
    return (term >> 6);
}

static void hash_helems(char* hname, helem_t** hvec, size_t hsize,
			helem_t* elems)
{
    int i = 0;

    DEBUGF("hash table %s size %u", hname, hsize);
    memset(hvec, 0, hsize*sizeof(helem_t*));

    while(elems[i].atm_ptr != NULL) {
	unsigned int hval = hash_atom(*elems[i].atm_ptr);
	unsigned int ix = hval % hsize;
	if (hvec[i] != NULL) DEBUGF("hash conflict %d", i);
	elems[i].next = hvec[ix];
	elems[i].hval = hval;
	hvec[ix] = &elems[i];
	i++;
    }
}

static int lookup_atom(helem_t** hvec, size_t hsize, ERL_NIF_TERM arg)
{
    unsigned int hval = hash_atom(arg);
    unsigned int ix = hval % hsize;
    helem_t* ptr = hvec[ix];
    while(ptr) {
	if (*ptr->atm_ptr == arg)
	    return (int) ptr->enm;
	ptr = ptr->next;
    }
    return -1;
}

#endif
