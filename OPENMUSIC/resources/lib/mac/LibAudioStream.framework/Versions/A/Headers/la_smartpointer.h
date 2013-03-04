/*
  Copyright (C) 2003  Grame

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

*/

#ifndef __la_smartpointer__
#define __la_smartpointer__

#include <cassert>
#include <stdio.h>

#ifdef WIN32
#pragma warning (disable : 4786)
#endif

#ifdef WIN32
# ifdef LIBAUDIOSTREAM_EXPORTS
#  define LA_EXPORT _declspec (dllexport)
# else
#  define LA_EXPORT _declspec (dllimport)
# endif
#elif __APPLE__
# ifdef LIBAUDIOSTREAM_EXPORTS
#  define LA_EXPORT __attribute__ ((visibility("default")))
# else
#  define LA_EXPORT 
# endif
#else
# define LA_EXPORT
#endif

/*!
\brief the base class for smart pointers implementation

	Any object that want to support smart pointers should
	inherit from the smartable class which provides reference counting
	and automatic delete when the reference count drops to zero.
*/
class LA_EXPORT la_smartable {
			
	public:
		//! gives the reference count of the object
		unsigned refs() const         { return refCount; }
		//! addReference increments the ref count and checks for refCount overflow
		void addReference()           { refCount++; assert(refCount != 0); }
		//! removeReference delete the object when refCount is zero		
		virtual void removeReference();
		
	protected:
		unsigned long refCount;	
		la_smartable() : refCount(0) {}
		la_smartable(const la_smartable&): refCount(0) {}
		//! destructor checks for non-zero refCount
		virtual ~la_smartable()    { assert (refCount == 0); }
		la_smartable& operator=(const la_smartable&) { return *this; }
};

class TCmdManager;

class la_smartable1 : public la_smartable {

	private:
		
		static void removeReferenceAux(la_smartable1* obj,long u1, long u2, long u3);
		static TCmdManager* fManager;

	public:
	
		void removeReference();
		static void Init();
		static void Destroy();
};

/*!
\brief the smart pointer implementation

	A smart pointer is in charge of maintaining the objects reference count 
	by the way of pointers operators overloading. It supports class 
	inheritance and conversion whenever possible.
\n	Instances of the SMARTP class are supposed to use \e smartable types (or at least
	objects that implements the \e addReference and \e removeReference
	methods in a consistent way).
*/
template<class T> class LA_SMARTP {
	private:
		//! the actual pointer to the class
		T* fSmartPtr;

	public:
		//! an empty constructor - points to null
		LA_SMARTP()	: fSmartPtr(0) {}
		//! build a smart pointer from a class pointer
		LA_SMARTP(T* rawptr) : fSmartPtr(rawptr)              {
			if (fSmartPtr) fSmartPtr->addReference(); 
		}
		//! build a smart pointer from an convertible class reference
		template<class T2> 
		LA_SMARTP(const LA_SMARTP<T2>& ptr) : fSmartPtr((T*)ptr) { if (fSmartPtr) fSmartPtr->addReference(); }
		//! build a smart pointer from another smart pointer reference
		LA_SMARTP(const LA_SMARTP& ptr) : fSmartPtr((T*)ptr)     { if (fSmartPtr) fSmartPtr->addReference(); }

		//! the smart pointer destructor: simply removes one reference count
		~LA_SMARTP()  { if (fSmartPtr) fSmartPtr->removeReference(); }
		
		//! cast operator to retrieve the actual class pointer
		operator T*() const  { return fSmartPtr;	}

		//! '*' operator to access the actual class pointer
		T& operator*() const {
			// checks for null dereference
			assert (fSmartPtr != 0);
			return *fSmartPtr;
		}
	
		//! operator -> overloading to access the actual class pointer
		T* operator->() const	{ 
			// checks for null dereference
			assert (fSmartPtr != 0);
			return fSmartPtr;
		}

		//! operator = that moves the actual class pointer
		template <class T2>
		LA_SMARTP& operator=(T2 p1_)	{ *this=(T*)p1_; return *this; }

		//! operator = that moves the actual class pointer
		LA_SMARTP& operator=(T* p_)	{
			// check first that pointers differ
			if (fSmartPtr != p_) {
				// increments the ref count of the new pointer if not null
				if (p_ != 0) p_->addReference();
				// decrements the ref count of the old pointer if not null
				if (fSmartPtr != 0) fSmartPtr->removeReference();
				// and finally stores the new actual pointer
				fSmartPtr = p_;
			}
			return *this;
		}
		//! operator = to support inherited class reference
		LA_SMARTP& operator=(const LA_SMARTP<T>& p_)                { return operator=((T *) p_); }
		//! dynamic cast support
		template<class T2> LA_SMARTP& cast(T2* p_)               { return operator=(dynamic_cast<T*>(p_)); }
		//! dynamic cast support
		template<class T2> LA_SMARTP& cast(const LA_SMARTP<T2>& p_) { return operator=(dynamic_cast<T*>(p_)); }
};

#endif
