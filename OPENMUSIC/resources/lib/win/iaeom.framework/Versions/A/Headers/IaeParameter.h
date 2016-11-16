/*
 *  @file iaeparameter.h
 *  @author iemo.Schwarz@ircam.fr
 *
 *  Created by Diemo Schwarz on 12.12.12.
 *  Copyright 2012 IMTR Ircam — Centre Pompidou. All rights reserved.
 *
 */


#ifndef _IAE_PARAMETER_H_
#define _IAE_PARAMETER_H_

extern "C" { 
#include "iaelog.h" 
}

#include "IaePreset.h"


///////////////////////////////////////////////////////////////////////////////
///
/// \defgroup Parameter Parameter Declaration
/// @{

/** base class of a declared IAE parameter, no template
    (N.B.: we need this non-template base class for forward declarations and pointers)
*/
class IaeParameter
{
public:
  // constructor
  IaeParameter (PresetManager &pm, int ord, const char *name) 
    : manager_(pm), order_(ord), name_(name), length_(0) // must be initialiser to assign reference, not copy object
  {
    //iae_db("IaeParameter %p ctor %s\n", this, name);
    pm.add_param(this);
  };
  virtual ~IaeParameter () {};
  
  virtual void recall_param (Preset &pst) = 0;
  virtual void mix_param (Preset &dst, Preset &src, float mixfactor) = 0;

  int get_length() { return length_; };
  //  PresetManager &get_manager() { return manager_; };
 
  /// set from float
  virtual void set_from_float (double v1, double v2 = 0, double v3 = 0) = 0;
  
protected:
  PresetManager &manager_;	// each parameter knows its manager
  int		 length_;	// value list length
  const std::string name_;		// (base) name of param
  int            order_;         // order for recall

  friend bool sort_by_order (const IaeParameter *left, const IaeParameter *right);
	
public:
  const char    *get_name() { return name_.c_str(); };
};


/** common parts of type-dependent templates for parameters
 */
template <typename TYPE>
class IaeParameterCommon : public IaeParameter
{
public:
  // constructor
  IaeParameterCommon (PresetManager &pm, const char *name, int len, int order) 
    : IaeParameter(pm, order, name) // must be initialiser to assign reference, not copy object
  {
    //iae_db("IaeParameterCommon %p ctor %s len %d\n", this, name, len);
    length_    = len;
  };
  virtual ~IaeParameterCommon () {};

  // set with args and apply can only be in derived templates
  // set(TYPE1 arg1, TYPE2, args, ...)
  virtual void apply(TYPE *vlist) = 0;

  // set parameter value in preset with list
  virtual void set (TYPE *vlist, Preset &pst) = 0;

  // set parameter value in preset with list
  virtual void set (int len, TYPE *vlist, Preset &pst) { };
    
  // set current
  void set (TYPE *vlist)	
  {
    set(vlist, manager_.current_); 
  }

  void set (int len, TYPE *vlist)	
  {
    set(len, vlist, manager_.current_); 
  }
  
  // get parameter value in preset with list
  bool get (TYPE *vlist, Preset &pst)
  {
    PresetItemCommon<TYPE> *item = (PresetItemCommon<TYPE> *)pst.getItem(name_);

    if (item)	    // retrieve current values
      item->getValues(vlist);
		
	
    return item != NULL;
  }
	
  // get parameter value and item length in preset with list
  bool get (int &len, TYPE *vlist, Preset &pst)
  {
    PresetItemCommon<TYPE> *item = (PresetItemCommon<TYPE> *)pst.getItem(name_);

    if (item)	    // retrieve current values
    {	
      item->getValues(vlist);
      len = item->length_;
    }
    else 
      len = length_; //if not corresponding item, use default length

    return item != NULL;
  }
	
  // get from current
  bool get (TYPE *vlist)
  {
    return get(vlist, manager_.current_);
  }
  
  // get specific element from current
  bool get (int index, TYPE *v)
  {
    return pick(index, v, manager_.current_);
  }
  
  bool pick(int index, TYPE *v, Preset &pst)
  {
    PresetItemCommon<TYPE> *item = (PresetItemCommon<TYPE> *)pst.getItem(name_);
    
    if (item)	    // retrieve current values
    {	
      item->getValue(index, v);
    }
    
    return item != NULL;
  }
};


/** generates parameter with constant length array of type \p TYPE
*/
template <typename CLASS, typename TYPE>
class IaeParameterArray : public IaeParameterCommon<TYPE>
{
  // with sub-inheritance via templates, we must tell the stubborn compiler which members we want to use
  // however, the base directly does NOT work:
  // using IaeParameter::length_;
  // using IaeParameter::name_;
  using IaeParameterCommon<TYPE>::length_;
  using IaeParameterCommon<TYPE>::name_;

  // shorthand for our base class template
  typedef IaeParameterCommon<TYPE> IaeParameterCommonArray;

private:
  void (CLASS::*setmethod_)(int len, TYPE *);

public:
  // constructor
  IaeParameterArray (PresetManager &pm, const char *name, 
		     int len,
		     void (CLASS::*setter)(int, TYPE *), int order = 0)
    : IaeParameterCommonArray(pm, name, len, order) // must be initialiser in base class to assign reference, not copy object
  {
    //iae_db("IaeParameterArray %p ctor %s len %d\n", this, name, len);
    setmethod_ = setter;
    count_ = 0;    // at initialization the aray is empty
  }

  void recall_param (Preset &pst)
  {
    TYPE *vlist = new TYPE [length_];
    int itemLength;

    // get current values
    if (this->get(itemLength, vlist, pst))		// todo: get pointer directly, don't copy
      apply(itemLength, vlist);				// call setter method on iae object

    delete vlist;
  }

  // set parameter value in preset with list
  void set (TYPE *vlist, Preset &pst)
  {
    // call setter method on iae object
    apply(vlist);
    
    // store values in current preset
    pst.setItem(name_, new PresetItemArray<TYPE>(length_, vlist));
  }

  void set (int len, TYPE *vlist, Preset &pst)
  {
    // call setter method on iae object
    apply(len, vlist);
    
    // store values in current preset
    pst.setItem(name_, new PresetItemArray<TYPE>(len, vlist));
  }
  
  void set_from_float (double v1, double v2, double v3)
  {
    TYPE vlist[3] = { Convert<TYPE>::fromFloat(v1), Convert<TYPE>::fromFloat(v2), Convert<TYPE>::fromFloat(v3) };
    set(length_, vlist);    
  }


  void mix_param (Preset &dst, Preset &pst, float mixfactor)
  {
    TYPE *vlist = new TYPE [length_];

    // get current values
    this->get(vlist, pst);		// todo: get pointer directly, don't copy

    delete vlist;
  }

  // tell the stubborn compiler not to mask the declarations of set in the base class
  using IaeParameterCommonArray::set;

  // apply parameter value to synth
  void apply (TYPE *vlist)
  {
    (static_cast<CLASS *>(IaeParameterCommonArray::manager_.iae_)->*setmethod_)(length_, vlist);
  }
	
  // apply parameter value to synth, knowing the number of parameters actually carried by the array
  void apply (int len, TYPE *vlist)
  {
	count_ = len;	// update the number of contained elements
	(static_cast<CLASS *>(IaeParameterCommonArray::manager_.iae_)->*setmethod_)(len, vlist);
  }
    
  // get the actual number of used elements in the array, out of the maximum number ( = length_ )
  int count()
  {
      return count_;
  }
    
/* array broken down to index calls???
    set (TYPE *value)
    {
	for (int i = 0; i < length_; i++)
	    setMethod(i, value[i]);
    }

private:
    (*Iaecuss::setMethod)(int index, TYPE value);
*/
protected:
    int count_;
};
/*TODO: IaeParameterList: non-fixed length array */


/** common parts of templates for fixed number of parameters
 */
template <int NVAL, typename CLASS, typename TYPE>
class IaeParameterFixed : public IaeParameterCommon <TYPE>
{    
  // with sub-inheritance via templates, we must tell the stubborn compiler which members we want to use
  using IaeParameterCommon<TYPE>::name_;

public:
  // constructor
  IaeParameterFixed (PresetManager &pm, const char *name, int order)
    : IaeParameterCommon<TYPE>(pm, name, NVAL, order) // must be initialiser in base class to assign reference, not copy object
  {
  }

  // set parameter value in preset with list
  virtual void set (TYPE *vlist, Preset &pst)
  {
    // call setter method on iae object
    this->apply(vlist);

    // store values in current preset
    pst.setItem(name_, new PresetItem<TYPE, NVAL>(vlist));
    //TODO: 1. don't create new item, but use pst.getItemOrCreate<TYPE, NVAL>(name_)->setValues(vlist)
    //TODO: 2. rearchitecture: don't store values in current preset on set, but on preset store 
  }
  
  void recall_param (Preset &pst)
  {
    TYPE vlist[NVAL];

    // get current values
    if(this->get(vlist, pst))		// todo: get pointer directly, don't copy
		this->apply(vlist);    // call setter method on iae object
	  //printf(" = %f\n", vlist[0]);
  }

  void mix_param (Preset &dst, Preset &pst, float mixfactor)
  {
    TYPE vlist[NVAL];		// values to mix from
    TYPE dlist[NVAL];		// values to mix into
    float mix[NVAL];		// mix accumulator
#if 0
    // get current values
    get(vlist, pst);		// todo: get pointer directly, don't copy

    // create preset item if not existing
    //TODO: remove this, create all items on preset creation
    ParameterList::iterator param;

    if (dst.item_exists(name_))
    { // mix param
      // get existing destination values
      get(dlist, dst);		// todo: get pointer directly, don't copy

      for (int i = 0; i < NVAL; i++)
	dlist[i] += vlist[i] /* weight_*/ * mixfactor;

      dst.setItem(name_, new PresetItem<TYPE, NVAL>(dlist));
    }
    else
    { // set new param
      for (int i = 0; i < NVAL; i++)
	dlist[i] = vlist[i] /* weight_*/ * mixfactor;

      dst.setItem(name_, new PresetItem<TYPE, NVAL>(dlist));
       }
#endif
  }
};



///////////////////////////////////////////////////////////////////////////////
///
/// \defgroup ParameterTemplates Parameter Templates, dependent on value type and number of values
/// @{

/** generates parameter of type \p type with one argument
 */
template <typename CLASS, typename TYPE>
class IaeParameter1 : public IaeParameterFixed<1, CLASS, TYPE>
{    
  // shorthand for our base class template
  typedef IaeParameterFixed<1, CLASS, TYPE> IaeParameterFixed1;

private:
  void (CLASS::*setmethod_)(TYPE);

public:
  // constructor
  IaeParameter1 (PresetManager &pm, const char *name,
		 void (CLASS::*setter)(TYPE), int order = 0)
    : IaeParameterFixed1(pm, name, order), // must be initialised in base class to assign reference, not copy object
      setmethod_(setter)
  { }

  // tell the stubborn compiler not to mask the declarations of set 
  // (in the hyper-base class containing them, they can't be inherited from the direct base class, it seems)
  using IaeParameterCommon<TYPE>::set;

  // set with args
  void set (TYPE v)
  {
    set(&v);
  }
  
  void set_from_float (double v1, double v2, double v3)
  {
    TYPE v = Convert<TYPE>::fromFloat(v1);
    set(&v);
  }
  
  // apply parameter value to synth
  void apply (TYPE *vlist)
  {
    // call setter method on iae object
    (static_cast<CLASS *>(IaeParameterFixed1::manager_.iae_)->*setmethod_)(vlist[0]);
  }
};


/** generates parameter of type \p type with two arguments
 */
template <typename CLASS, typename TYPE>
class IaeParameter2 : public IaeParameterFixed<2, CLASS, TYPE>
{
  // shorthand for our base class template
  typedef IaeParameterFixed<2, CLASS, TYPE> IaeParameterFixed2;

private:
  void (CLASS::*setmethod_)(TYPE, TYPE);

public:
  // constructor
  IaeParameter2 (PresetManager &pm, 
		 const char *name, const char *suf1, const char *suf2, 
		 void (CLASS::*setter)(TYPE, TYPE), int order = 0)
    : IaeParameterFixed2(pm, name, order), // must be initialiser in base class to assign reference, not copy object
      setmethod_(setter)
  { }

  // tell the stubborn compiler not to mask the declarations of set in the base class
  using IaeParameterCommon<TYPE>::set;
  
  // set with args
  void set (TYPE v1, TYPE v2)
  {
    TYPE vlist[2] = {v1, v2};
    set(vlist);
  }

  void set_from_float (double v1, double v2, double v3)
  {
    TYPE vlist[2] = { Convert<TYPE>::fromFloat(v1), Convert<TYPE>::fromFloat(v2) };
    set(vlist);    
  }
  
  // apply parameter value to synth
  void apply (TYPE *vlist)
  {
    (static_cast<CLASS *>(IaeParameterFixed2::manager_.iae_)->*setmethod_)(vlist[0], vlist[1]);
  }
};



/** generates parameter of type \p type with three arguments
 */
template <typename CLASS, typename TYPE>
class IaeParameter3 : public IaeParameterFixed<3, CLASS, TYPE>
{
  // shorthand for our base class template
  typedef IaeParameterFixed<3, CLASS, TYPE> IaeParameterFixed3;

private:
  void (CLASS::*setmethod_)(TYPE, TYPE, TYPE);

public:
  // constructor
  IaeParameter3 (PresetManager &pm, const char *name, 
		 void (CLASS::*setter)(TYPE, TYPE, TYPE), int order = 0)
    : IaeParameterFixed3(pm, name, order) // must be initialiser in base class to assign reference, not copy object
  {
    init("1", "2", "3", setter);
  }

  IaeParameter3 (PresetManager &pm, const char *name, 
		 const char *suf1, const char *suf2, const char *suf3, 
		 void (CLASS::*setter)(TYPE, TYPE, TYPE), int order = 0)
    : IaeParameterFixed3(pm, name, order)
  {
    init(suf1, suf2, suf3, setter);
  }

  void init(const char *suf1, const char *suf2, const char *suf3, 
	    void (CLASS::*setter)(TYPE, TYPE, TYPE))
  {
    //todo: store suffixes
    setmethod_ = setter;
  }

  // tell the stubborn compiler not to mask the declarations of set in the base class
  using IaeParameterCommon<TYPE>::set;

  // set with args
  void set (TYPE v1, TYPE v2, TYPE v3)
  {
    TYPE vlist[3] = {v1, v2, v3};
    set(vlist);
  }

  void set_from_float (double v1, double v2, double v3)
  {
    TYPE vlist[3] = { Convert<TYPE>::fromFloat(v1), Convert<TYPE>::fromFloat(v2), Convert<TYPE>::fromFloat(v3) };
    set(vlist);    
  }

  // apply parameter value to synth
  void apply (TYPE *vlist)
  {
    // call setter method on iae object
    (static_cast<CLASS *>(IaeParameterFixed3::manager_.iae_)->*setmethod_)(vlist[0], vlist[1], vlist[2]);
  }
};

/// @}

/// @}


/** EMACS **
 * Local variables:
 * mode: c++
 * c-basic-offset:2
 * End:
 */

#endif /*_IAE_PARAMETER_H_*/
