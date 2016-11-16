/**
 *
 * @file mubutypes.h
 * @author Norbert.Schnell@ircam.fr
 * 
 * @brief MuBu type definitions
 * 
 * Copyright (C) 2009-2014 by IRCAM – Centre Pompidou, Paris, France.
 * All rights reserved.
 * 
 */

#ifndef MUBU_TYPES_H
#define MUBU_TYPES_H

#ifdef WIN32
#include <malloc.h>
#define snprintf sprintf_s
/* nick-nack for MSVS */
#if defined(MUBUDLL_EXPORTS)
#define MUBU_API __declspec(dllexport)
#elif !defined(MUBU_STATIC)
#define MUBU_API __declspec(dllimport)
#else
#define MUBU_API extern
#endif

#else

/* no nick-nack here */
#define MUBU_API extern

#endif

#include "stdint.h"

/***************************************************************** 
 *
 *  MuBu container and track
 *
 */
typedef struct MuBuContainerSt MuBuContainerT;
typedef struct MuBuTrackSt MuBuTrackT;
typedef struct MuBuTrackDescriptionSt MuBuTrackDescriptionT;

/***************************************************************** 
 *
 *  primitive numeric data type
 *
 */
#pragma mark -
#pragma mark platform numeric data type

typedef float MuBuDataT;

/***************************************************************** 
 *
 *  time type
 *
 */
#pragma mark -
#pragma mark platform time data type

typedef double MuBuTimeT;

#define MuBuTimeAbsMax DBL_MAX
#define MuBuTimeAbsMin -DBL_MAX

/***************************************************************** 
 *
 *  time type
 *
 */
#pragma mark -
#pragma mark platform time data type

typedef enum MuBuFileErrorE
{
  MuBuFileErrorNone = 0,
  MuBuFileErrorGeneric = -1,
  MuBuFileErrorInvalidPath = -2,
  MuBuFileErrorCannotOpen = -3,
  MuBuFileErrorInvalidFormat = -4,
  MuBuFileErrorInvalidNumTracks = -5,
  MuBuFileErrorCannotAppend = -6,
  MuBuFileErrorNoContent = -7,
  MuBuFileErrorOutOfMemory = -8,
  MuBuFileErrorReadWrite = -9
} MuBuFileErrorT;

/***************************************************************** 
 *
 *  platform includes
 *
 */
#pragma mark -
#pragma mark platform includes

#if (MUBU_MAXMSP > 0) /* Max/MSP */
#include "ext.h"
#include "ext_obex.h"
#include "ext_user.h"
#endif

/***************************************************************** 
 *
 *  platform malloc
 *
 */
#pragma mark -
#pragma mark platform malloc

#if (MUBU_MAXMSP > 0) /* Max/MSP */
#define mubuMem_alloc(s) malloc(s)
#define mubuMem_realloc(p, s) realloc((p), (s))
#define mubuMem_free(p) free(p)

#else /* standard C */
#define mubuMem_alloc(s) malloc(s)
#define mubuMem_realloc(p, s) realloc((p), (s))
#define mubuMem_free(p) free(p)

#endif

/***************************************************************** 
 *
 *  thread locks
 *
 */
#pragma mark -
#pragma mark platform thread locks

#if (MUBU_MULTITHREADING > 0)

#include <pthread.h>

typedef pthread_rwlock_t MuBuLockT;
#define mubuLock_init(p) pthread_rwlock_init((p), NULL)
#define mubuLock_deinit(p) pthread_rwlock_destroy(p)
#define mubuLock_exclusive(p) pthread_rwlock_wrlock(p)
#define mubuLock_shared(p) pthread_rwlock_rdlock(p)
#define mubuLock_try(p) pthread_rwlock_tryrdlock(p)
#define mubuLock_unlock(p) pthread_rwlock_unlock(p)

typedef pthread_mutex_t MuBuMutexT;
#define mubuMutex_init(p) pthread_mutex_init(p, NULL)
#define mubuMutex_initRecursive(p) do { \
  pthread_mutexattr_t recursiveMutexAttribute; \
  pthread_mutexattr_init(&recursiveMutexAttribute); \
  pthread_mutexattr_settype(&recursiveMutexAttribute, PTHREAD_MUTEX_RECURSIVE); \
  pthread_mutex_init(p, &recursiveMutexAttribute); \
} while(0)
#define mubuMutex_deinit(p) pthread_mutex_destroy(p)
#define mubuMutex_lock(p) pthread_mutex_lock(p)
#define mubuMutex_try(p) pthread_mutex_trylock(p)
#define mubuMutex_unlock(p) pthread_mutex_unlock(p)

#else

typedef void *MuBuLockT;
#define mubuLock_init(p)
#define mubuLock_deinit(p)
#define mubuLock_exclusive(p)
#define mubuLock_shared(p)
#define mubuLock_try(p) 0
#define mubuLock_unlock(p)

typedef void *MuBuMutexT;
#define mubuMutex_init(p)
#define mubuMutex_initRecursive(p)
#define mubuMutex_deinit(p)
#define mubuMutex_lock(p)
#define mubuMutex_try(p)
#define mubuMutex_unlock(p)

#endif

/***************************************************************** 
 *
 *  platform atomic instructions
 *
 */
#pragma mark -
#pragma mark platform atomic instructions

#if (MUBU_MULTITHREADING > 0)

#ifdef WIN32 /* Windows */
#include <intrin.h>
#include <inttypes.h>
#pragma intrinsic (_InterlockedIncrement)
#pragma intrinsic (_InterlockedDecrement)
#pragma intrinsic (_InterlockedCompareExchange)
#define mubuAtomic_incrInt32(p) (_InterlockedIncrement((volatile int32_t *)p))
#define mubuAtomic_decrInt32(p) (_InterlockedDecrement((volatile int32_t *)p))

#ifdef _WIN64
#define mubuAtomic_casLong(p, o, n) (_InterlockedCompareExchange64((int64_t volatile *)(p), (int64_t)(n), (int64_t)(o)) == (int64_t)(o))
#else
#define mubuAtomic_casLong(p, o, n) (_InterlockedCompareExchange((LONG volatile *)(p), (LONG)(n), (LONG)(o)) == (LONG)(o))
#endif

#elif defined __APPLE__ /* Mac OS X */
#include <libkern/OSAtomic.h>

#define mubuAtomic_incrInt32(p) (OSAtomicIncrement32Barrier((int32_t *)(p)))
#define mubuAtomic_decrInt32(p) (OSAtomicDecrement32Barrier((int32_t *)(p)))
#define mubuAtomic_casLong(p, o, n) (OSAtomicCompareAndSwapLongBarrier((long)(o), (long)(n), (long *)(p)))

#elif defined __linux__ /* Linux */
#ifdef __GNUC__
#define mubuAtomic_incrInt32(p) __sync_fetch_and_add((p), 1)
#define mubuAtomic_decrInt32(p) __sync_fetch_and_sub((p), 1)
#define mubuAtomic_casLong(p, o, n) __sync_bool_compare_and_swap((p), (o), (n))

#else /* Linux without GCC */
#error no atomic increment/decrement on Linux without GCC yet
#endif

#else /* others */
#error no atomic increment/decrement on this platform

#endif /* platforms */

#else /* without multi-threading */
#define mubuAtomic_incrInt32(p) ((*(p))++)
#define mubuAtomic_decrInt32(p) ((*(p))--)
#define mubuAtomic_casLong(p, o, n) ((*(p) == (o))? (*(p) = (n), 1): 0)

#endif /* multi-threading */

/***************************************************************** 
 *
 *  platform symbols
 *
 */
#pragma mark -
#pragma mark platform symbols

#if (MUBU_MAXMSP > 0) /* Max/MSP */
#define MUBU_SYMBOLS 0
typedef t_symbol *MuBuSymT;
#define mubuSymId(s) (gensym((char *)s))
#define mubuSymStr(s) ((s)->s_name)

#else /* other than Max/MSP */
#define MUBU_SYMBOLS 1
typedef const char *MuBuSymT;
MuBuSymT mubuSymId(const char *str);
#define mubuSymStr(s) (s)

#endif

/***************************************************************** 
 *
 *  platform atoms (generic values)
 *
 */
#pragma mark -
#pragma mark platform atoms

#if (MUBU_MAXMSP > 0) /* Max/MSP */

typedef enum MuBuValTypeIdE {
  MuBuValTypeVoid = A_NOTHING, 
  MuBuValTypeInt = A_LONG,
  MuBuValTypeFloat = A_FLOAT,
  MuBuValTypeSymbol = A_SYM,
  MuBuValTypeString = 99,
  MuBuValTypePointer = A_OBJ
} MuBuValTypeIdT;

typedef union word MuBuWordT;

#define mubuWord_setInt(p, v) ((p)->w_long = (v))
#define mubuWord_setFloat(p, v) ((p)->w_float = (v))
#define mubuWord_setSymbol(p, v) ((p)->w_sym = (v))
#define mubuWord_setString(p, v) ((p)->w_sym = gensym(v))
#define mubuWord_setPointer(p, v) ((p)->w_obj = (struct object *)(v))

#define mubuWord_getInt(p) ((p)->w_long)
#define mubuWord_getFloat(p) ((p)->w_float)
#define mubuWord_getSymbol(p) ((p)->w_sym)
#define mubuWord_getString(p) ((p)->w_sym->s_name)
#define mubuWord_getPointer(p) ((void *)(p)->w_obj)

typedef t_atom MuBuValT;

#define mubuVal_setVoid(p) (mubuWord_setInt(&(p)->a_w, 0), (p)->a_type = MuBuValTypeVoid)
#define mubuVal_setInt(p, v) (mubuWord_setInt(&(p)->a_w, (v)), (p)->a_type = MuBuValTypeInt)
#define mubuVal_setFloat(p, v) (mubuWord_setFloat(&(p)->a_w, (v)), (p)->a_type = MuBuValTypeFloat)
#define mubuVal_setSymbol(p, v) (mubuWord_setSymbol(&(p)->a_w, (v)), (p)->a_type = MuBuValTypeSymbol)
#define mubuVal_setString(p, v) (mubuWord_setString(&(p)->a_w, (v)), (p)->a_type = MuBuValTypeString)
#define mubuVal_setPointer(p, v) (mubuWord_setPointer(&(p)->a_w, (v)), (p)->a_type = MuBuValTypePointer)

#define mubuVal_isVoid(p) ((p)->a_type == A_NOTHING)
#define mubuVal_isFloat(p) ((p)->a_type == A_FLOAT) 
#define mubuVal_isInt(p) ((p)->a_type == A_LONG) 
#define mubuVal_isNumber(p) ((p)->a_type == A_LONG || (p)->a_type == A_FLOAT) 
#define mubuVal_isSymbol(p) ((p)->a_type == A_SYM)
#define mubuVal_isString(p) ((p)->a_type == A_SYM)
#define mubuVal_isPointer(p) ((p)->a_type == A_OBJ)

#define mubuVal_getInt(p) mubuWord_getInt(&(p)->a_w)
#define mubuVal_getFloat(p) mubuWord_getFloat(&(p)->a_w)
#define mubuVal_getNumberInt(p) (mubuVal_isInt(p) ? mubuVal_getInt(p) : (int)mubuVal_getFloat(p))
#define mubuVal_getNumberFloat(p) (mubuVal_isFloat(p) ? mubuVal_getFloat(p) : (double)mubuVal_getInt(p))
#define mubuVal_getSymbol(p) mubuWord_getSymbol(&(p)->a_w)
#define mubuVal_getString(p) mubuWord_getString(&(p)->a_w)
#define mubuVal_getPointer(p) mubuWord_getPointer(&(p)->a_w)

#define mubuVal_getTypeID(p) ((MuBuValTypeIdT)(p)->a_type)

#else /* other than Max/MSP */

typedef enum MuBuValTypeIdE {
  MuBuValTypeVoid = 0, 
  MuBuValTypeInt,
  MuBuValTypeFloat,
  MuBuValTypeNumber,
  MuBuValTypeSymbol,
  MuBuValTypeString,
  MuBuValTypePointer
} MuBuValTypeIdT;

typedef union MuBuWordU {
  int i;
  float f;
  MuBuSymT sym;
  void *ptr;
  const char *str;
} MuBuWordT;

#define mubuWord_setInt(p, v) ((p)->i = (v))
#define mubuWord_setFloat(p, v) ((p)->f = (v))
#define mubuWord_setSymbol(p, v) ((p)->sym = (v))
#define mubuWord_setString(p, v) ((p)->str = (const char *)(v))
#define mubuWord_setPointer(p, v) ((p)->ptr = (void *)(v))

#define mubuWord_getInt(p) ((p)->i)
#define mubuWord_getFloat(p) ((p)->f)
#define mubuWord_getSymbol(p) ((p)->sym)
#define mubuWord_getString(p) ((p)->str)
#define mubuWord_getPointer(p) ((p)->ptr)

typedef struct MuBuValSt {
  enum MuBuValTypeIdE type;
  MuBuWordT word;
} MuBuValT;

#define mubuVal_setVoid(p) (mubuWord_setInt(&(p)->word, 0), (p)->type = MuBuValTypeVoid)
#define mubuVal_setInt(p, v) (mubuWord_setInt(&(p)->word, (v)), (p)->type = MuBuValTypeInt)
#define mubuVal_setFloat(p, v) (mubuWord_setFloat(&(p)->word, (v)), (p)->type = MuBuValTypeFloat)
#define mubuVal_setSymbol(p, v) (mubuWord_setSymbol(&(p)->word, (v)), (p)->type = MuBuValTypeSymbol)
#define mubuVal_setString(p, v) (mubuWord_setString(&(p)->word, (v)), (p)->type = MuBuValTypeString)
#define mubuVal_setPointer(p, v) (mubuWord_setPointer(&(p)->word, (v)), (p)->type = MuBuValTypePointer)

#define mubuVal_isVoid(p) ((p)->type == MuBuValTypeVoid)
#define mubuVal_isFloat(p) ((p)->type == MuBuValTypeFloat) 
#define mubuVal_isInt(p) ((p)->type == MuBuValTypeInt) 
#define mubuVal_isNumber(p) ((p)->type == MuBuValTypeInt || (p)->type == MuBuValTypeFloat) 
#define mubuVal_isSymbol(p) ((p)->type == MuBuValTypeSymbol)
#define mubuVal_isString(p) ((p)->type == MuBuValTypeString)
#define mubuVal_isPointer(p) ((p)->type == MuBuValTypePointer)

#define mubuVal_getInt(p) mubuWord_getInt(&(p)->word)
#define mubuVal_getFloat(p) mubuWord_getFloat(&(p)->word)
#define mubuVal_getNumberInt(p) (mubuVal_isInt(p) ? mubuVal_getInt(p) : (int)mubuVal_getFloat(p))
#define mubuVal_getNumberFloat(p) (mubuVal_isFloat(p) ? mubuVal_getFloat(p) : (double)mubuVal_getInt(p))
#define mubuVal_getSymbol(p) mubuWord_getSymbol(&(p)->word)
#define mubuVal_getString(p) mubuWord_getString(&(p)->word)
#define mubuVal_getPointer(p) mubuWord_getPointer(&(p)->word)

#define mubuVal_getTypeID(p) ((p)->type)

#endif

/***************************************************************** 
 *
 *  time (for polling of changes)
 *
 */
#pragma mark -
#pragma mark platform time

#if (MUBU_MAXMSP > 0) /* Max/MSP */
#define mubuTime_get() systimer_gettime()

#else /* other than Max/MSP */
#define mubuTime_get() 0.0

#endif

#endif
