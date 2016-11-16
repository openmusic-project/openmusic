/**
 *
 * @file iaepreset.h
 * @author Diemo.Schwarz@ircam.fr
 * 
 * @brief IMTR audio engine preset and parameter implementation
 * 
 * Copyright (C) 2012 by IMTR IRCAM – Centre Pompidou, Paris, France.
 * All rights reserved.
 * 
 */

#ifndef _IAE_PRESET_H_
#define _IAE_PRESET_H_

#include "ListLoader-ListDumper.h"

#include "assert.h"
#include "stdio.h"
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <iomanip>
#include <typeinfo>


//#define MAX_PRESET_ARRAY 8

template <typename TYPE>
class Convert
{
public:
  // to be specialised for non-numeric item types: 
  // convert meaningfully from float to item type after mixing/interpolation
  static TYPE fromFloat (double v)
  {
    return (TYPE) v;
  };
};

// specialisations of conversion (enums seem to work with standard cast)
template <>
bool Convert<bool>::fromFloat (double v);

template <>
char * Convert<char *>::fromFloat (double v);

template <>
std::string Convert<std::string>::fromFloat (double v);



/** Most generic preset item, to be collected in hashes
 */

class PresetItemBase
{
protected:  // make sure no-one creates a bare PresetItemBase object
  PresetItemBase() {};
  
public:
  virtual ~PresetItemBase() {};

  int	length_;
  int	elemsize_;
  float	weight_;
  
  //virtual void getValues (/*out*/ void *v);
  /*{
   printf("\n+++++++++++++++++++++++++++mem copy\n");
   memcpy(v, values(), length_ * elemsize_);  
   }*/
  
  int  getLength () { return length_; };

  virtual void setValuesFromFloat (double *v) = 0;

  virtual void zero() = 0;
  
  virtual const char * type() = 0;
  
  /*
private:
  virtual void *values() = 0;*/
};


/** common fields of a preset item
 */
template <typename TYPE>
class PresetItemCommon : public PresetItemBase
{
public:
  //TYPE	*value_;

  virtual void getValues (/*out*/ TYPE *v) = 0;
  
  virtual void getValue (int index, /*out*/ TYPE *v) = 0;
	
  virtual void zero()			   = 0;
	
  //virtual const char * type()	= 0;
  const char * type()
  {
    typeid(TYPE).name();
  }
  
//private:
//  virtual void *values() = 0;
};

/** template to declare a preset item with @p LEN arguments of type @p TYPE.
*/
template <typename TYPE, int LEN>
class PresetItem : public PresetItemCommon<TYPE>
{
public:
  TYPE	value_[LEN];

  PresetItem (TYPE *v, float w = 1)
  {
    this->length_	= LEN;
    this->elemsize_	= sizeof(TYPE);
    this->weight_	= w;
    setValues(v);
  }

  void setValues (TYPE *v)
  {
    for (int i = 0; i < LEN; i++)
      value_[i] = v[i];  
  }

  void setValuesFromFloat (double *v)
  {
    for (int i = 0; i < LEN; i++)
      value_[i] = Convert<TYPE>::fromFloat(v[i]);
  }
  
  void getValues (/*out*/ TYPE *v)
  {
    for (int i = 0; i < LEN; i++)
      v[i] = this->value_[i];
  }
  
  void getValue (int index, /*out*/ TYPE *v)
  {
    *v = value_[index];
  }

  void zero()
  {
    for (int i = 0; i < LEN; i++)
      value_[i] = (TYPE) 0;  
  }

 const char * type()
 {
   return typeid(TYPE).name();
 }
/*	
private:
  void *values()
  {
    return value_;
  }*/
};

/** template to declare a preset item with a constant size array of type @p TYPE.
*/
template <typename TYPE>
class PresetItemArray : public PresetItemCommon<TYPE>
{
public:
  TYPE	*value_;

  PresetItemArray (int len, TYPE *v, float w = 1)
  {
    this->length_     = len;
    this->elemsize_   = sizeof(TYPE);
    this->weight_     = w;
    this->value_      = new TYPE[this->length_ ];
    setValues(v);
  }
	
  ~PresetItemArray()
  {
    delete value_;
    value_ = NULL;
  }

private:
  // forbid copy constructor and assignment because of pointer members
  PresetItemArray (PresetItemArray &) {};
  PresetItemArray &operator= (PresetItemArray &) {};
  
public:
  void setValues (TYPE *v)
  {
    for (int i = 0; i < this->length_; i++)
      value_[i] = v[i];  
  }
  
  void setValuesFromFloat (double *v)
  {
    for (int i = 0; i < this->length_; i++)
      value_[i] = Convert<TYPE>::fromFloat(v[i]);
  }
  
  void getValues (/*out*/ TYPE *v)
  {
    for (int i = 0; i < this->length_; i++)
      v[i] = value_[i];  

  }
  
  void getValue (int index,/*out*/ TYPE *v)
  {
      *v = value_[index];  
  }

  void zero()
  {
    for (int i = 0; i < this->length_; i++)
      value_[i] = (TYPE) 0;  
  }

  const char * type()
  {
    //PresetItemCommon<TYPE>::type();
    typeid(TYPE).name();
  }
/*	
private:
  void *values()
  {
    return value_;
  }*/
};



typedef std::map<const std::string, PresetItemBase *> Items;

/** \brief class holding a preset, i.e. collection of parameter values.
*/
class Preset
{
public:
  // constructor
  Preset ()
  {
    //printf("ctor Preset: items_ %p\n", &items_);
  }
  
  // destructor
  ~Preset ()
  {
    //printf("dtor Preset: items_ %p\n", &items_);
  }

  // copy/assignment
  Preset &operator= (const Preset p)
  {
    items_ = p.items_;
  }

  int numItems ()
  {
    return items_.size();
  }

  void clear ()
  {
    items_.clear();
  }
	
	Items::iterator items_begin()
	{
		return items_.begin();
	}
	
	Items::iterator items_end()
	{
		return items_.end();
	}
  
  bool item_exists (const std::string &name)
  {
    Items::iterator it = items_.find(name);

    return it != items_.end();
  }
  
    
  void zero ()
  {
    std::map<const std::string, PresetItemBase *>::iterator iter;

    // for each item: zero value
    for (iter = items_.begin(); iter != items_.end(); iter++)
      iter->second->zero();    
  }
    
  void setItem (const std::string name, PresetItemBase *item)
  {
    items_[name] = item;
  }
  
  PresetItemBase* getItem(std::string name)
  {
    //printf("Preset::getItem(%s): items_ %p\n", name.c_str(), &items_);
	  Items::iterator itm = items_.find(name);
	  if (itm != items_.end()) 
		  return items_[name];
	  else 
		  return NULL;
	  //TODO: create empty item
  }

  /*
    void mixParam (PresetItemBase item, double mixweight)
    {
	if (items_.Contains(item.parameter_))
	    // parameter already present in mix: add double weighted new ingredient
	    getParam(item.parameter_).value_ += item.value_ * item.weight_ * mixweight;
	else
	    // parameter_ not yet preset in mix: create and double weight it
	    items_.Add(item.parameter_, 
		       new PresetItemBase(item.parameter_, item.value_ * item.weight_ * mixweight));
    }
  */
private:
  Items items_; // dict of PresetItem
  // todo: std::vector of PresetItem*, fixed size and indices according to IaeParameter declaration
};



///////////////////////////////////////////////////////////////////////////////
///
/// \defgroup PresetManager Preset Management
/// @{

class IaeParameter;
class IaeSynth;

typedef std::vector<IaeParameter *>		ParameterList;
typedef std::map<std::string, IaeParameter *>   ParameterMap;
typedef std::map<std::string, Preset>           PresetHash;
typedef std::map<int, std::string>		PresetIndexHash;


/** \incapsulation class, holding the data structure used to store and manipulate loaded presets
 */
class PresetList
{
public:
	//constructor
	PresetList()
	{
		presetHash_ = PresetHash();
		presetHash_.clear();
		
		presetIndexHash_ = PresetIndexHash();
		presetIndexHash_.clear();
	}
	
	//---------------------------------------------
	//exposition of the main methods of hash map and vector
	int size()
	{
		return presetHash_.size();
	}
	 
	PresetHash::iterator find(const char *name)
	{
		return presetHash_.find(name);
	}
	 
	PresetHash::iterator find(int index)
	{

		PresetIndexHash::iterator name = presetIndexHash_.find(index);
		if(name != presetIndexHash_.end())
			return presetHash_.find(name->second);
		else
			return presetHash_.end();
	}
	
	
	PresetHash::iterator begin()
	{
		return presetHash_.begin();
	}
	 
	PresetHash::iterator end()
	{
		return presetHash_.end();
	}
	
	//acces by name [map style]
	Preset&  operator[](const char *name)
	{
		PresetHash::iterator pst = presetHash_.find(name);
		assert(pst != presetHash_.end());	// no add capabilities, must use "put()" method
		return presetHash_[name];
	}
	
	//access by index [vactor style]
	Preset&  operator[](int index)
	{
		PresetIndexHash::iterator pstIndex = presetIndexHash_.find(index);
		assert(pstIndex != presetIndexHash_.end());	// no add capabilities, must use "put()" method
		return presetHash_[presetIndexHash_[index]];
	}
	
	void put(Preset pst, const char *name, int index = -1)
	{
		PresetHash::iterator pstIt = presetHash_.find(name);

		//add new preset
		if(pstIt == presetHash_.end())
		{
			//if no indx is provided, the count of current elements+1 is used as index [*WARNING* could lead to key duplicates]
			if(index == -1)
				index = presetHash_.size()+1;	// +1 since preset indexing starts from 1
			presetIndexHash_[index] = name;
			presetHash_[name] = pst;
		}
		else // overwrite
		{
			//overwrite preset
			presetHash_[name] = pst;
			
			//if has to change the index
			if(index != -1)
			{
				//erase previous index<->name coupling
				PresetIndexHash::iterator ind;
				for (ind = presetIndexHash_.begin(); ind != presetIndexHash_.end(); ind++) 
				{
					if(std::strcmp((const char*) (ind->second.c_str()), name)==0)
					{
						presetIndexHash_.erase(ind);
						break;
					}
				}
				//create a new coupling, with same name but new index
				presetIndexHash_[ind->first] = name;
			}
			
		}
		
	}
	
	void clear()
	{
		presetHash_.clear();
		presetIndexHash_.clear();
	}
	
	std::string get_name(int index)
	{
		return presetIndexHash_[index+1];	// +1 since preset indexing starts from 1
	}

private:
	PresetHash	presetHash_;		//map: <name, preset>
	PresetIndexHash presetIndexHash_;	//vector to store index of each name
};


class ListLoader;


/******************************************************************************
 *
 * @brief preset manager base class
 *
 */
class PresetManager
{
public:
  // constructor
  PresetManager ();
  virtual ~PresetManager () {};

private:
  // forbid copy constructor and assignment because of pointer members
  PresetManager (PresetManager &) {};
  PresetManager &operator= (PresetManager &) {};
  
public:
  IaeSynth *get_iae() { return iae_; };
  
#pragma mark parameter handling

  ParameterList &get_parameter_list() { return params_; };

  /// check if name is a declared parameter
  bool is_parameter (std::string name);
  bool is_parameter (const char *name)   { return is_parameter(std::string(name)); };

  /// get parameter by name
  /// @return NULL if name does not exist
  IaeParameter *get_parameter (const char *name);

  /// set parameter by name, up to 3 values
  bool set_parameter (const char *name, double value1, double value2 = 0, double value3 = 0);

  // declare parameter (called by iaeparameter constructor only)
  void add_param (IaeParameter *p);
  
  // to be called after all iaeparameter declarations: sort by order
  void finalize_params ();

  void list_param ();

#pragma mark preset handling
  
  PresetItemBase *getCurrent (std::string name)
  {
    return current_.getItem(name);
  }

  // store current settings under given name
  int store (const char *name);

  // recall settings from given preset
  int recall (const char *name); //todo: recall through index
  int recall (int index);

  void mix_begin();
  void mix_end();

  // mix settings from given preset into mixer register
  int mix_preset (const char *name, float mixfactor);

  void recall_preset (Preset &pst);	

  int presetListSize()
  {
	  return presets_.size();
  }
	
  //load external preset list, returns true only if loading procedure is successful
  int loadList(const char * name)
  {
	  //call the generic preset list load method on field listLoader_
	  if(listLoader_.loadList(name)) //returns true if successful
	  {
		  copyList();
		  return presets_.size();
	  }
	  else 
		return -1;
  }
	
  //dump current preset list onto external source, returns true only if dumping procedure is successful
  bool dumpList(const char * name)
  {
	  PresetWrapper pw = writeList();
	  return listDumper_.dumpList(pw, name);
  }
	
  const char * presetName(int index)
  {
	
	  return presets_.get_name(index).c_str();
  }
	
private:
  //copy the records loaded from the external preset list into the local preset list data structure
  void copyList();
	
  template<typename TYPE> 
  void parseItem(ItemData item, TYPE * val, const char * delimiters);
  
  void parseStringItem(ItemData item, std::string * val, const char * delimiters);
  void parseStringItem(ItemData item, std::vector<std::string> vector, const char * delimiters);
  void trimSpaces(std::string& str);
	
  template<typename TYPE> 
  void addItem(ItemData item, TYPE * val, Preset &preset);
	
  //these methods, starting from the local preset list data structure, write down the list of records that will be dumped 
  PresetWrapper writeList();
  ItemWrapper getItemsFromPreset(Preset preset);
	
  template<typename TYPE> 
  void setItemDataValueNumber(TYPE* val, ItemData &iData, PresetItemCommon<TYPE> * itm);
	
  void setItemDataValueBool(bool* val, ItemData &iData, PresetItemCommon<bool> * itm);
  void setItemDataValueString(std::string* val, ItemData &iData, PresetItemCommon<std::string> * itm);
  void setItemDataValue(ItemData &iData, std::string itemValString);


	
  //---------------------------------------------	
protected:
  ParameterList params_;		// list of parameters, in order of recall
  ParameterMap  params_map_; // dictionary for parameter lookup

private:
  PresetList    presets_;		// local list of presets
  ListLoader    listLoader_;	// handler to load external list of presets
  ListDumper    listDumper_;	// handler to load external list of presets

protected:
  static PresetManager static_presets_;

  //TODO: friend class IaeParameter;
public:
	
  //TODO: IaeSynth as template parameter
  IaeSynth *iae_;	// our owner

  //TODO: per voice????
  Preset current_;	// current parameter values
  Preset mixer_;	// parameter mix accumulator (all items in float)
  double mixsum_;
  int	 nmix_;
}; // end class PresetManager

/// @} end group Preset Management

/** EMACS **
 * Local variables:
 * mode: c++
 * c-basic-offset:2
 * End:
 */

#endif /* _IAE_PRESET_H_ */
