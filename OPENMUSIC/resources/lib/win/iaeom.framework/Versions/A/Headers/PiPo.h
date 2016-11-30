/**
 *
 * @file PiPo.h
 * @author Norbert.Schnell@ircam.fr
 *
 * @brief Plugin Interface for Processing Objects
 *
 * Copyright (C) 2012-2014 by IRCAM – Centre Pompidou, Paris, France.
 * All rights reserved.
 *
 */
#ifndef _PIPO_
#define _PIPO_

#include <string>
#include <vector>
#include <functional>
#include <cstring>
#include <cstdio>

#include <typeinfo>
#include <map>

#ifdef WIN32
#define strcasecmp _stricmp
#define M_PI 3.14159265358979323846264338328 /**< pi */
#endif


typedef float PiPoValue;


//TODO: unify with maxpipohost.h
#define PIPO_MAX_LABELS 1024

struct PiPoStreamAttributes
{
  int hasTimeTags;
  double rate;
  double offset;
  unsigned int dims[2];	// width, height (by pipo convention)
  const char **labels;
  unsigned int numLabels;
  bool hasVarSize;
  double domain;
  unsigned int maxFrames;
  int labels_alloc; //< allocated size of labels, -1 for no (outside) allocation
  
  PiPoStreamAttributes (int numlabels = -1)
  {
    init(numlabels);
  }

  void init (int _numlab = -1)
  {
    this->hasTimeTags = false;
    this->rate = 1000.0;
    this->offset = 0.0;
    this->dims[0] = 1;	// width
    this->dims[1] = 1;	// height
    this->labels = NULL;
    this->numLabels = 0;
    this->labels_alloc = _numlab;
    this->hasVarSize = false;
    this->domain = 0.0;
    this->maxFrames = 1;

    if (_numlab >= 0)
      labels = new const char *[_numlab];
  };

  ~PiPoStreamAttributes()
  {
    if (labels  &&  labels_alloc > 0)
      delete [] labels;
  };

  /** append string pointer array to labels array
   */
  void concat_labels (const char **_labels, unsigned int _width)
  {
    if (this->labels_alloc < 0)
    {
      printf("Warning: PiPoStreamAttributes::concat_labels: can't concat %d labels to char ** with %d labels allocated from the outside\n", _width, this->numLabels);
      _width = 0;
    }
    
    if (this->numLabels + _width > this->labels_alloc)
    {
      printf("Warning: PiPoStreamAttributes::concat_labels: label overflow prevented (trying to concat %d to %d used of %d)\n", _width, this->numLabels, this->labels_alloc);
	_width = this->labels_alloc - this->numLabels;
    }
      
    if (_labels != NULL)
      memcpy(this->labels + this->numLabels, _labels, _width * sizeof(const char *));
    else
      for (int i = 0; i < _width; i++)
	this->labels[i + this->numLabels] = "unnamed";	//TODO: invent numbered column, beware of memory!
    
    this->numLabels += _width;
  }

};


class PiPo
{

/** @mainpage 

PiPo is a simple plugin API for modules processing streams of multi-dimensional data such as audio, audio descriptors, or gesture and motion data. The current version of the interface is limited to unary operations. Each PiPo module receives and produces a single stream. The elements of a stream are time-tagged or regularly sampled scalars, vectors, or two-dimensional matrices. 

More information http://imtr.ircam.fr/imtr/PiPo

\section sec_api  PiPo API Overview

The PiPo API consists of an abstract class of a few virtual methods for propagating stream attributes (see below), frames, and additional processing control through a series of modules:

- Propagating stream attributes
- Propagating frames
- Reset stream processing
- Finalize stream processing
- Propagate the change of a parameter requiring redefining the output stream attributes 


\subsection sec_impl Implementation of a New PiPo Module

The minimal module must derive from the class PiPo and implement at least the \ref streamAttributes and \ref frames methods:

- In \ref streamAttributes, all initialisation can be done, as all input stream parameters (attributes) are known. The output stream parameters are passed on to the receiving module via \ref propagateStreamAttributes.
- In \ref frames, only data processing and, when needed, buffering should be done.  Output frames are passed on with \ref propagateFrames.

If the module can produce additional output data after the end of the input data, it must implement \ref finalize, from within which more calls to \ref propagateFrames can be made, followed by a mandatory call to \ref propagateFinalize.

If the module keeps internal state or buffering, it should implement the \ref reset method to put itself into a clean state.

The utility function \ref signalError can be used to pass an error message to the host.

The utility function \ref signalWarning can be used to pass a warning message to the host.


\subsection sec_attr Module Attributes or Parameters

The template class PiPo::Attr permits to define scalar, enum, or variable or fixed size vector attributes of a pipo module that are exposed to the host environment.

The are initialised in the module constructor with a short name, a description, a flag if a change of value means the fundamental stream parameters must be reset (if true, \ref streamAttributes will be called again for the whole chain), and a default value.

Their value can be queried in \ref streamAttributes or \ref frames (in real-time hosts, an attributes value can change over time) with PiPo::Attr::get().

\subsection sec_example Example of a Minimal PiPo Module


\code

class PiPoGain : public PiPo
{
private:
  std::vector<PiPoValue> buffer;

public:
  PiPoScalarAttr<double> factor;
  
  PiPoGain (Parent *parent, PiPo *receiver = NULL)
  : PiPo(parent, receiver), 
    factor(this, "factor", "Gain Factor", false, 1.0)
  { }
  
  ~PiPoGain (void)
  { }

  int streamAttributes (bool hasTimeTags, double rate, double offset,
                        unsigned int width, unsigned int height,
                        const char **labels, bool hasVarSize,
                        double domain, unsigned int maxFrames)
  {
    // A general pipo can not work in place, we need to create an output buffer
    buffer.resize(width * height * maxFrames);

    return propagateStreamAttributes(hasTimeTags, rate, offset, width, height,
                                     labels, hasVarSize, domain, maxFrames);
  }
  
  int frames (double time, double weight, PiPoValue *values,
              unsigned int size, unsigned int num)
  {
    double f = factor.get(); // get gain factor here, as it could change while running
    PiPoValue *ptr = &buffer[0];
	
    for (unsigned int i = 0; i < num; i++)
    {
      for (unsigned int j = 0; j < size; j++)
	ptr[j] = values[j] * f;

      ptr    += size;
      values += size;
    }
    
    return propagateFrames(time, weight, &buffer[0], size, num);
  }
};
\endcode


\section sec_api_details PiPo API Details

\subsection sec_stream_attr PiPo Stream Attributes

PiPo streams are a sequences of frames characterized by a set of attributes. A PiPo module defines the attributes of its output stream when receiving the attributes of the input stream.

Each module can configure its internal state depending on the attributes of the input stream (e.g. memory allocation and pre-calculated state variables) before propagating its output stream attributes to the next module.

This way, the attributes of the input stream are propagated through a series of PiPo modules before starting the actual stream processing.

In summary, a PiPo stream is described by the following attributes:

- a boolean representing whether the elements of the stream are time-tagged
- frame rate (highest average rate for time-tagged streams)
- lag of the output stream relative to the input
- frame width (also number of channels or data matrix columns)
- frame height (or number of matrix rows)
- labels (for the frame channels or columns)
- a boolean representing whether the frames have a variable height (respecting the given frame height as maximum)
- extent of a frame in the given domain (e.g. duration or frequency range)
- maximum number of frames in a block exchanged between two modules 

*/

    
public:
  class Attr;
  
  /***********************************************
   *
   *  PiPo Parent
   * TODO: rename PiPoHost ?
   */

  class Parent
  {
  public:
    /** called by pipo when an attribute with "changesstream" is set */
    virtual void streamAttributesChanged(PiPo *pipo, PiPo::Attr *attr) { };
    
    /** called by pipo to signal error in parameters */
    virtual void signalError(PiPo *pipo, std::string errorMsg) { };

    /** called by pipo to signal warning in parameters */
    virtual void signalWarning(PiPo *pipo, std::string errorMsg) { };
  };
  
protected:
  Parent *parent;
  std::vector<PiPo *> receivers; /**< list of receivers */

private:
  std::vector<Attr *> attrs; /**< list of attributes */
  
public:
  PiPo(Parent *parent, PiPo *receiver = NULL)
  : receivers(), attrs()
  {
    this->parent = parent;
    
    if(receiver != NULL)
      this->receivers.push_back(receiver);
  };
  
  PiPo(const PiPo &other)
  {
    this->parent = other.parent;
  }
  
  virtual ~PiPo(void) { };
  
  /**
   * @brief Sets PiPo parent.
   *
   * @param parent PiPo parent
   */
  virtual void setParent(Parent *parent) { this->parent = parent; };
  
  /**
   * @brief Propagates a module's output stream attributes to its receiver.
   *
   * This method is called in the streamAttributes() method of a PiPo module.
   *
   * @param hasTimeTags a boolean representing whether the elements of the stream are time-tagged
   * @param rate the frame rate (highest average rate for time-tagged streams)
   * @param offset the lag of the output stream relative to the input
   * @param width the frame width (also number of channels or data matrix columns)
   * @param height the frame height (or number of matrix rows)
   * @param labels optional labels for the frames' channels or columns
   * @param hasVarSize a boolean representing whether the frames have a variable height (respecting the given frame height as maximum)
   * @param domain extent of a frame in the given domain (e.g. duration or frequency range)
   * @param maxFrames maximum number of frames in a block exchanged between two modules
   * @return used as return value of the calling method
   */
  int propagateStreamAttributes(bool hasTimeTags, double rate, double offset, unsigned int width, unsigned int height, const char **labels, bool hasVarSize, double domain, unsigned int maxFrames)
  {
    int ret = 0;
    
    for(unsigned int i = 0; i < this->receivers.size(); i++)
    {
      ret = this->receivers[i]->streamAttributes(hasTimeTags, rate, offset, width, height, labels, hasVarSize, domain, maxFrames);
      
      if(ret < 0)
        break;
    }
    
    return ret;
  };
  
  /**
   * @brief Propagates the reset control event.
   *
   * This method is called in the reset() method of a PiPo module.
   *
   * @return used as return value of the calling method
   */
  int propagateReset(void)
  {
    int ret = -1;
    
    for(unsigned int i = 0; i < this->receivers.size(); i++)
    {
      ret = this->receivers[i]->reset();
      
      if(ret < 0)
        break;
    }
    
    return ret;
  }
  
  /**
   * @brief Propagates a module's output frames to its receiver.
   *
   * This method is called in the frames() method of a PiPo module.
   *
   * @param time time-tag for a single frame or a block of frames
   * @param weight weight for this frame (currently unused)
   * @param values interleaved frames values, row by row (interleaving channels or columns), frame by frame
   * @param size total size of each frame (number of values = width * height)
   * @param num number of frames
   * @return used as return value of the calling method
   */
  int propagateFrames(double time, double weight, PiPoValue *values, unsigned int size, unsigned int num)
  {
    int ret = -1;
    
    for(unsigned int i = 0; i < this->receivers.size(); i++)
    {
      ret = this->receivers[i]->frames(time, weight, values, size, num);
      
      if(ret < 0)
        break;
    }
    
    return ret;
  }
  
  /**
   * @brief Propagates the finalize control event.
   *
   * This method is called in the finalize() method of a PiPo module.
   *
   * @return used as return value of the calling method
   */
  int propagateFinalize(double inputEnd)
  {
    int ret = -1;
    
    for(unsigned int i = 0; i < this->receivers.size(); i++)
    {
      ret = this->receivers[i]->finalize(inputEnd);
      
      if(ret < 0)
        break;
    }
    
    return ret;
  }
  
  /**
   * @brief Gets a PiPo modules receiver (call only by the PiPo host)
   *
   * @return receiver PiPo module receiving this module's output stream
   */
  PiPo *getReceiver(unsigned int index = 0)
  {
    if(index < this->receivers.size())
      return this->receivers[index];
    
    return NULL;
  };
  
  /**
   * @brief Sets a PiPo modules receiver (call only by the PiPo host)
   *
   * @param receiver PiPo module receiving this module's output stream
   * @param add receiver (versus clear and set first)
   */
  virtual void setReceiver(PiPo *receiver, bool add = false)
  {
    if(add)
    {
      if(receiver != NULL)
        this->receivers.push_back(receiver);
    }
    else
    {
      this->receivers.clear();
      
      if(receiver != NULL)
        this->receivers.push_back(receiver);
    };
  }
  
  /**
   * @brief Configures a PiPo module according to the input stream attributes and propagate output stream attributes
   *
   * PiPo module:
   * Any implementation of this method requires a propagateStreamAttributes() method call and returns its return value, typically like this:
   *
   * \code 
   *	return this->propagateStreamAttributes(hasTimeTags, rate, offset, width, height, labels, hasVarSize, domain, maxFrames);
   * \endcode
   *
   * PiPo host:
   * A terminating receiver module provided by a PiPo host handles the final output stream attributes and usally returns 0.
   *
   * @param hasTimeTags a boolean representing whether the elements of the stream are time-tagged
   * @param rate the frame rate (highest average rate for time-tagged streams, sample rate for audio input)
   * @param offset the lag of the output stream relative to the input
   * @param width the frame width (number of channels for audio or data matrix columns)
   * @param height the frame height (or number of matrix rows, always 1 for audio)
   * @param labels optional labels for the frames' channels or columns (can be NULL)
   * @param hasVarSize a boolean representing whether the frames have a variable height (respecting the given frame height as maximum)
   * @param domain extent of a frame in the given domain (e.g. duration or frequency range)
   * @param maxFrames maximum number of frames in a block exchanged between two modules (window size for audio)
   * @return 0 for ok or a negative error code (to be specified), -1 for an unspecified error
   */
  virtual int streamAttributes(bool hasTimeTags, double rate, double offset, unsigned int width, unsigned int height, const char **labels, bool hasVarSize, double domain, unsigned int maxFrames) = 0;
  
  /**
   * @brief Resets processing (optional)
   *
   * PiPo module:
   * Any implementation of this method requires a propagateReset() method call and returns its return value.
   *
   * PiPo host:
   * A terminating receiver module provided by a PiPo host usally simply returns 0.
   *
   * @return 0 for ok or a negative error code (to be specified), -1 for an unspecified error
   */
  virtual int reset(void)
  {
    return this->propagateReset();
  };
  
  /**
   * @brief Processes a single frame or a block of frames
   *
   * PiPo module:
   * An implementation of this method may call propagateFrames(), typically like this:
   *
   * \code
   *	return this->propagateFrames(time, weight, values, size, num);
   * \endcode
   *
   * PiPo host:
   * A terminating receiver module provided by a PiPo host handles the received frames and usally returns 0.
   *
   * @param time time-tag for a single frame or a block of frames
   * @param weight weight associated to frame or block
   * @param values interleaved frames values, row by row (interleaving channels or columns), frame by frame
   * @param size total size of each of all frames (size = number of elements = width * height = number of channels for audio)
   * @param num number of frames (number of samples for audio input)
   * @return 0 for ok or a negative error code (to be specified), -1 for an unspecified error
   */
  virtual int frames(double time, double weight, PiPoValue *values, unsigned int size, unsigned int num) = 0;
  
  /**
   * @brief Signals segment start or end
   *
   * PiPo module:
   * An implementation of this method calls propagateFrames() at the end of the segment.
   *
   * In the case of two sucessive calls to segment(), the second call implitly ends the last segment.
   *
   * If the module did not receive any frames - at all or since the last segment end -, the method should
   * return 0 to the call segment(0.0, end) without calling propagateFrames().
   * This permits the host to check whether a module implements the segment method or not.
   *
   * \code
   
   if(this->started)
   {
   // do what is to be done to finalize the segment description
   this->propagateFrames(time, weight, values, size, num);
   this->started = false;
   }
   
   if(start)
   {
   // do what is to be done to initialize the segment description
   }
   
   return 0;

   * \endcode
   *
   * @param time time of segment start of end
   * @param start flag, true for segment start, false for segment end
   * @return 0 for ok or a negative error code (to be specified), -1 for an unspecified error
   */
  virtual int segment(double time, bool start) { return -1; };
  
  /**
   * @brief Finalizes processing (optinal)
   *
   * PiPo module:
   * Any implementation of this method requires a propagateFinalize() method call and returns its return value.
   *
   * PiPo host:
   * A terminating receiver module provided by a PiPo host usally simply returns 0.
   *
   * @param inputEnd end time of the finalized input stream
   * @return 0 for ok or a negative error code (to be specified), -1 for an unspecified error
   */
  virtual int finalize(double inputEnd)
  {
    return this->propagateFinalize(inputEnd);
  };
  
  void streamAttributesChanged(Attr *attr)
  {
    if(this->parent != NULL)
      this->parent->streamAttributesChanged(this, attr);
  }

  /** signal error message to be output by the host
   */
  void signalError(std::string errorMsg)
  {
    if(this->parent != NULL)
      this->parent->signalError(this, errorMsg);
    else
      printf("PiPo::signalError (not parent): %s\n", errorMsg.c_str());
  }

    /** signal warning message to be output by the host
   */
  void signalWarning(std::string errorMsg)
  {
    if(this->parent != NULL)
      this->parent->signalWarning(this, errorMsg);
    else
      printf("PiPo::signalWarning (not parent): %s\n", errorMsg.c_str());
  }


  
  /***********************************************
   *
   *  PiPo Attributes
   *
   */
public:
  enum Type { Undefined, Bool, Enum, Int, Float, Double, String };

  // dummy enum used for specialization of templates
  enum Enumerate { };
    
  // meta-type a la Max :
  class Atom
  {
  private:
      PiPo::Type type;
      union _data {
          const char *str;
          double dbl;
          int itg;
      } data;
  public:
      Atom(const char *s)   { this->type = String; this->data.str = s; }
      Atom(double d)        { this->type = Double; this->data.dbl = d; }
      Atom(int i)           { this->type = Int; this->data.itg = i; }
      friend bool operator==(Atom &at1, Atom &at2)
      {
          return ((at1.isNumber() && at2.isNumber() && at1.getDouble() == at2.getDouble())
                  || (at1.type == String && at1.type == at2.type && strcmp(at1.getString(), at2.getString()) == 0));
      }
      friend bool operator!=(Atom &at1, Atom &at2)
      {
          return !(at1 == at2);
      }
      bool          isNumber()  { return (type == Int || type == Double); }
      bool          isString()  { return type == String; }
      PiPo::Type    getType()   { return type; }
      int           getInt()    { return ((type == Int) ? this->data.itg : ((type == Double) ? (int)(this->data.dbl) : 0)); }
      double        getDouble() { return ((type == Double) ? this->data.dbl : ((type == Int) ? (double)(this->data.itg) : 0.)); }
      const char *  getString() { return ((type == String) ? this->data.str : ""); }
  };
  
  class Attr
  {
  private:
    PiPo *pipo; /**< owner PiPo */
    unsigned int index;
    const char *name; /**< attribute name */
    const char *descr; /**< short description */
    enum Type type;
    bool changesStream;
    
  public:
    /**
     * PiPo attribute base class
     */
    Attr(PiPo *pipo, const char *name, const char *descr, const std::type_info *type, bool changesStream)
    {
      this->pipo = pipo;
      this->index = pipo->attrs.size();
      this->name = name;
      this->descr = descr;
      
      if(type == &typeid(bool))
        this->type = Bool;
      else if(type == &typeid(enum Enumerate))
        this->type = Enum;
      else if(type == &typeid(int))
        this->type = Int;
      else if(type == &typeid(float))
        this->type = Float;
      else if(type == &typeid(double))
        this->type = Double;
      else if(type == &typeid(std::string) || type == &typeid(const char *))
        this->type = String;
      else
        this->type = Undefined;
      
      this->changesStream = changesStream;
      
      pipo->attrs.push_back(this);
    };
    
    ~Attr(void) { };
    
    void setIndex(unsigned int index) { this->index = index; };
    void setName(const char *name) { this->name = name; };
    void setDescr(const char *descr) { this->descr = descr; };
    
    unsigned int getIndex(void) { return this->index; };
    const char *getName(void) { return this->name; };
    const char *getDescr(void) { return this->descr; };
    enum Type getType(void) { return this->type; };
    bool doesChangeStream(void) { return this->changesStream; };
    
    virtual void clone(Attr *other) = 0;
    
    virtual unsigned int setSize(unsigned int size) = 0;
    virtual unsigned int getSize(void) = 0;
    
    virtual void set(unsigned int i, int val, bool silently = false) = 0;
    virtual void set(unsigned int i, double val, bool silently = false) = 0;
    virtual void set(unsigned int i, const char *val, bool silently = false) = 0;
    virtual int getInt(unsigned int i) = 0;
    virtual double getDbl(unsigned int i) = 0;
    virtual const char *getStr(unsigned int i) = 0;
    
    virtual std::vector<const char *> *getEnumList(void) { return NULL; };
    
    void changed(bool silently = false) { if (!silently && this->changesStream) this->pipo->streamAttributesChanged(this); };
    void rename(const char *name) { this->name = name; };
  };
  
  /**
   * PiPo enumerator attribute base class
   */
  class EnumAttr : public Attr
  {
    struct strCompare : public std::binary_function<const char *, const char *, bool> {
      bool operator() (const char *str1, const char *str2) const { return std::strcmp(str1, str2) < 0; }
    };
    
    std::vector<const char *>enumList;
    std::vector<const char *>enumListDoc;
    std::map<const char *, unsigned int, strCompare> enumMap;
    
  public:
    EnumAttr(PiPo *pipo, const char *name, const char *descr, const std::type_info *type, bool changesStream) :
    Attr(pipo, name, descr, type, changesStream),
    enumList(), enumListDoc(), enumMap()
    {
    };
    
    void addEnumItem(const char *item, const char *doc = "undocumented")
    {
      unsigned int idx = this->enumList.size();
      
      this->enumList.push_back(item);
      this->enumListDoc.push_back(doc);
      this->enumMap[item] = idx;
    };
    
    std::vector<const char *> *getEnumList(void)
    {
      return &this->enumList;
    };
    
    int getEnumIndex(const char *tag)
    {
      if(tag != NULL && this->enumMap.find(tag) != this->enumMap.end())
        return this->enumMap[tag];
      
      return -1;
    };
    
    const char *getEnumTag(unsigned int idx)
    {
      if (idx < this->enumList.size())
        return this->enumList[idx];
      
      return NULL;
    };
    
  protected:
    int clipEnumIndex(int index)
    {
      if(index < 0)
        index = 0;
      else if(index >= (int)this->enumList.size())
        index = this->enumList.size() - 1;
      
      return index;
    };
  };
  
  /**
   * @brief att attribute
   */
  void addAttr(PiPo *pipo, const char *name, const char *descr, Attr *attr, bool clear = false)
  {
    if(clear)
      this->attrs.clear();
    
    /* overwrite index, name, and description */
    attr->setIndex(pipo->attrs.size());
    attr->setName(name);
    attr->setDescr(descr);
    
    /* add to attr list */
    this->attrs.push_back(attr);
  };
  
  /**
   * @brief Gets PiPo attribute by index
   *
   * @param index attribute index
   * @return reference to PiPo attribute (NULL for invalid attribute index)
   */
  Attr *getAttr(unsigned int index)
  {
    if(index < this->attrs.size())
      return this->attrs[index];
    
    return NULL;
  };
  
  /**
   * @brief Gets PiPo attribute by name
   *
   * @param name attribute name
   * @return reference to PiPo attribute (NULL for invalid attribute name)
   */
  Attr *getAttr(const char *name)
  {
    for(unsigned int i = 0; i < this->attrs.size(); i++)
    {
      if(strcasecmp(this->attrs[i]->getName(), name) == 0)
        return this->attrs[i];
    }
    
    return NULL;
  };

    /**
   * @brief Gets PiPo attribute by qualified name
   *
   * @param piponame pipo module name in pipo chain
   * @param name attribute name
   * @return reference to PiPo attribute (NULL for invalid attribute name)
   */
  Attr *getAttr(const char *piponame, const char *name)
  {
    std::string qname(piponame);

    qname += ".";
    qname += name;

    return getAttr(qname.c_str());
  };
  
  bool setAttr(unsigned int index, int value, bool silently = false)
  {
    Attr *attr = getAttr(index);
    
    if(attr != NULL)
    {
      attr->set(0, value, silently);
      
      return true;
    }
    
    return false;
  }
  
  bool setAttr(unsigned int index, int *values, unsigned int numValues, bool silently = false)
  {
    Attr *attr = getAttr(index);
    
    if(attr != NULL)
    {
      unsigned int size = attr->getSize();
      
      if(numValues > size)
        numValues = size;
      
      for(unsigned int i = 0; i < numValues; i++)
        attr->set(i, values[i], silently);
      
      return true;
    }
    
    return false;
  }
  
  bool setAttr(unsigned int index, double val, bool silently = false)
  {
    Attr *attr = getAttr(index);
    
    if(attr != NULL)
    {
      attr->set(0, val, silently);
      
      return true;
    }
    
    return false;
  }
  
  bool setAttr(unsigned int index, double *values, unsigned int numValues, bool silently = false)
  {
    Attr *attr = getAttr(index);
    
    if(attr != NULL)
    {
      unsigned int size = attr->getSize();
      
      if(numValues > size)
        numValues = size;
      
      for(unsigned int i = 0; i < numValues; i++)
        attr->set(i, values[i], true);
      
      attr->changed(silently);
      
      return true;
    }
    
    return false;
  }
  
  /**
   * @brief Gets number of attributes
   *
   * @return number of attributes
   */
  unsigned int getNumAttrs(void)
  {
    return this->attrs.size();
  };
  
  /**
   * @brief Copies current parent and attributes values
   *
   * @param other PiPo to clone
   */
  void cloneAttrs(PiPo *other)
  {
    for(unsigned int i = 0; i < other->attrs.size(); i++)
      this->attrs[i]->clone(other->attrs[i]);
  };
  
  /**
   * @brief Copies current value(s) of given attribute
   *
   * @param other PiPo to clone
   */
  void cloneAttr(PiPo::Attr *attr)
  {
    unsigned int index = attr->getIndex();
    
    this->attrs[index]->clone(attr);
  };
};



/***********************************************
 *
 *  Scalar Attribute
 *
 */
template <typename TYPE>
class PiPoScalarAttr : public PiPo::Attr
{
private:
  TYPE value;
  
public:
  PiPoScalarAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, TYPE initVal = (TYPE)0) :
  Attr(pipo, name, descr, &typeid(TYPE), changesStream)
  {
    this->value = initVal;
  }
  
  void set(TYPE value, bool silently = false) { this->value = value; this->changed(silently); };
  TYPE get(void) { return this->value; };
  
  void clone(Attr *other) { this->value = (dynamic_cast<PiPoScalarAttr<TYPE> *>(other))->value; };
  
  unsigned int setSize(unsigned int size) { return this->getSize(); };
  unsigned int getSize(void) { return 1; };
  
  void set(unsigned int i, int val, bool silently = false) { if(i == 0) this->value = (TYPE)val; this->changed(silently); };
  void set(unsigned int i, double val, bool silently = false) { if(i == 0) this->value = (TYPE)val; this->changed(silently); };
  void set(unsigned int i, const char *val, bool silently = false) { };
  
  int getInt(unsigned int i = 0) { return (int)this->value; };
  double getDbl(unsigned int i = 0) { return (double)this->value; };
  const char *getStr(unsigned int i = 0) { return NULL; };
};

template <>
class PiPoScalarAttr<const char *> : public PiPo::Attr
{
private:
    const char * value;
    
public:
    PiPoScalarAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, const char * initVal = (const char *)0) :
    Attr(pipo, name, descr, &typeid(const char *), changesStream)
    {
        this->value = initVal;
    }
    
    void set(const char * value) { this->value = value; };
    const char *get(void) { return this->value; };
    
    void clone(Attr *other) { *this = *(static_cast<PiPoScalarAttr<const char *> *>(other)); };
    
    unsigned int setSize(unsigned int size) { return this->getSize(); };
    unsigned int getSize(void) { return 1; };
    
    void set(unsigned int i, int val, bool silently = false) {  };
    void set(unsigned int i, double val, bool silently = false) {  };
    void set(unsigned int i, const char *val, bool silently = false) { if(i == 0) this->value = val; this->changed(silently); };
    
    int getInt(unsigned int i = 0) { return 0; };
    double getDbl(unsigned int i = 0) { return 0; };
    const char *getStr(unsigned int i = 0) { return this->value; };
};

template <>
class PiPoScalarAttr<enum PiPo::Enumerate> : public PiPo::EnumAttr
{
private:
  unsigned int value;
  
public:
  PiPoScalarAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, unsigned int initVal = NULL) :
  EnumAttr(pipo, name, descr, &typeid(enum PiPo::Enumerate), changesStream)
  {
    this->value = initVal;
  };
  
  void set(unsigned int value, bool silently = false) { this->value = clipEnumIndex(value); this->changed(silently); };
  void set(const char *value, bool silently = false) { this->value = this->getEnumIndex(value); this->changed(silently); };
  unsigned int get(void) { return this->value; };
  
  void clone(Attr *other) { this->value = (dynamic_cast<PiPoScalarAttr<enum PiPo::Enumerate> *>(other))->value; };

  unsigned int setSize(unsigned int size) { return this->getSize(); };
  unsigned int getSize(void) { return 1; };
  
  void set(unsigned int i, int val, bool silently = false) { if(i == 0) this->value = clipEnumIndex((unsigned int)val); this->changed(silently); };
  void set(unsigned int i, double val, bool silently = false) { if(i == 0) this->value = clipEnumIndex((unsigned int)val); this->changed(silently); };
  void set(unsigned int i, const char *val, bool silently = false) { if(i == 0) this->value = getEnumIndex(val); this->changed(silently); };
  
  int getInt(unsigned int i = 0) { return (int)this->value; };
  double getDbl(unsigned int i = 0) { return (double)this->value; };
  const char *getStr(unsigned int i = 0) { return this->getEnumTag(this->value); };
};

/***********************************************
 *
 *  Fixed Size Array Attribute
 *
 */
/* waiting for C++11 */
template< class TYPE, std::size_t SIZE>
class arrayClass
{
  TYPE values[SIZE];
  static const int size = SIZE;
  
public:
  TYPE const& operator [] (unsigned int index) const { return this->values[index]; };
  TYPE& operator [] (unsigned int index) { return &this->values[index]; };
};

template <typename TYPE, unsigned int SIZE>
class PiPoArrayAttr : public PiPo::Attr, public arrayClass<TYPE, SIZE>
{
public:
  PiPoArrayAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, TYPE initVal = (TYPE)0) :
  Attr(pipo, name, descr, &typeid(TYPE), changesStream),
  arrayClass<TYPE, SIZE>()
  {
    for(unsigned int i = 0; i < SIZE; i++)
      (*this)[i] = initVal;
  }
  
  void clone(Attr *other) { *(dynamic_cast<arrayClass<TYPE, SIZE> *>(this)) = *(dynamic_cast<arrayClass<TYPE, SIZE> *>(other)); };

  unsigned int setSize(unsigned int size) { return this->getSize(); };
  unsigned int getSize(void) { return SIZE; }
  
  void set(unsigned int i, int val, bool silently = false)
  {
    if(i < SIZE)
      (*this)[i] = (TYPE)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, double val, bool silently = false)
  {
    if(i < SIZE)
      (*this)[i] = (TYPE)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, const char *val, bool silently = false) { };
  
  int getInt(unsigned int i)
  {
    if(i >= SIZE)
      i = SIZE - 1;
    
    return (int)(*this)[i];
  };
  
  double getDbl(unsigned int i)
  {
    if(i >= SIZE)
      i = SIZE - 1;
    
    return (double)(*this)[i];
  };
  
  const char *getStr(unsigned int i)
  {
    if (i < SIZE)
      i = SIZE - 1;
    
    return (const char *)(*this)[i];
  };
};

template <unsigned int SIZE>
class PiPoArrayAttr<enum PiPo::Enumerate, SIZE> : public PiPo::EnumAttr, public arrayClass<unsigned int, SIZE>
{
public:
  PiPoArrayAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, unsigned int initVal = NULL) :
  EnumAttr(pipo, name, descr, &typeid(enum PiPo::Enumerate), changesStream),
  arrayClass<unsigned int, SIZE>()
  {
    for(unsigned int i = 0; i < this->size; i++)
      this->value[i] = initVal;
  }
  
  ~PiPoArrayAttr(void) { free(this->value); }
  
  void clone(Attr *other) { *(dynamic_cast<arrayClass<unsigned int, SIZE> *>(this)) = *(dynamic_cast<arrayClass<unsigned int, SIZE> *>(other)); };

  unsigned int setSize(unsigned int size) { return this->getSize(); };
  unsigned int getSize(void) { return SIZE; }
  
  void set(unsigned int i, int val, bool silently = false)
  {
    if(i < SIZE)
      (*this)[i] = (unsigned int)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, double val, bool silently = false)
  {
    if(i < SIZE)
      (*this)[i] = (unsigned int)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, const char *val, bool silently = false)
  {
    if(i < SIZE)
      (*this)[i] = getEnumIndex(val);
    
    this->changed(silently);
  };
  
  int getInt(unsigned int i)
  {
    if(i >= SIZE)
      i = SIZE - 1;
    
    return (int)(*this)[i];
  };
  
  double getDbl(unsigned int i)
  {
    if(i >= SIZE)
      i = SIZE - 1;
    
    return (double)(*this)[i];
  };
  
  const char *getStr(unsigned int i)
  {
    if (i < SIZE)
      return this->getEnumTag(this->value[i]);
    
    return NULL;
  };
};

/***********************************************
 *
 *  Var Size Attribute
 *
 */
template <typename TYPE>
class PiPoVarSizeAttr : public PiPo::Attr, public std::vector<TYPE>
{
public:
  PiPoVarSizeAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, unsigned int size = 0, TYPE initVal = (TYPE)0) :
  Attr(pipo, name, descr, &typeid(TYPE), changesStream),
  std::vector<TYPE>(size, initVal)
  {
  }
  
  void clone(Attr *other) { *(dynamic_cast<std::vector<TYPE> *>(this)) = *(dynamic_cast<std::vector<TYPE> *>(other)); };

  unsigned int setSize(unsigned int size) { this->resize(size, (TYPE)0); return size; };
  unsigned int getSize(void) { return this->size(); }
  
  void set(unsigned int i, int val, bool silently = false)
  {
    if (i >= this->size())
      setSize(i + 1);
    
    (*this)[i] = (TYPE)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, double val, bool silently = false)
  {
    if (i >= this->size())
      setSize(i + 1);
    
    (*this)[i] = (TYPE)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, const char *val, bool silently = false) { };
  
  int getInt(unsigned int i)
  {
    if(i >= this->size())
      i = this->size() - 1;
    
    return (int)(*this)[i];
  };
  
  double getDbl(unsigned int i)
  {
    if(i >= this->size())
      i = this->size() - 1;
    
    return (double)(*this)[i];
  };
  
  const char *getStr(unsigned int i) { return NULL; };
  
  TYPE *getPtr()	// return pointer to first data element
  {
    return &((*this)[0]);
  };
};


template <>
class PiPoVarSizeAttr<enum PiPo::Enumerate> : public PiPo::EnumAttr, public std::vector<unsigned int>
{
public:
  PiPoVarSizeAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, unsigned int size = 0, unsigned int initVal = NULL) :
  EnumAttr(pipo, name, descr, &typeid(enum PiPo::Enumerate), changesStream),
  std::vector<unsigned int>(size, 0)
  {
    for(unsigned int i = 0; i < this->size(); i++)
      (*this)[i] = initVal;
  };
  
  void clone(Attr *other) { *(dynamic_cast<std::vector<unsigned int> *>(this)) = *(dynamic_cast<std::vector<unsigned int> *>(other)); };

  unsigned int setSize(unsigned int size) { this->resize(size, 0); return size; };
  unsigned int getSize(void) { return this->size(); }
  
  void set(unsigned int i, int val, bool silently = false)
  {
    if (i >= this->size())
      setSize(i + 1);
    
    (*this)[i] = (unsigned int)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, double val, bool silently = false)
  {
    if (i >= this->size())
      setSize(i + 1);
    
    (*this)[i] = (unsigned int)val;
    
    this->changed(silently);
  };
  
  void set(unsigned int i, const char *val, bool silently = false)
  {
    if (i >= this->size())
      setSize(i + 1);
    
    (*this)[i] = getEnumIndex(val);
    
    this->changed(silently);
  };
  
  int getInt(unsigned int i)
  {
    if(i >= this->size())
      i = this->size() - 1;
    
    return (int)(*this)[i];
  };
  
  double getDbl(unsigned int i)
  {
    if(i >= this->size())
      i = this->size() - 1;
    
    return (double)(*this)[i];
  };
  
  const char *getStr(unsigned int i)
  {
    if (i < this->size())
      return this->getEnumTag((*this)[i]);
    
    return NULL;
  };
};


template<>
class PiPoVarSizeAttr<PiPo::Atom> : public PiPo::Attr, public std::vector<PiPo::Atom>
{
public:
    PiPoVarSizeAttr(PiPo *pipo, const char *name, const char *descr, bool changesStream, unsigned int size = 0, int initVal = 0) :
    Attr(pipo, name, descr, &typeid(const char *), changesStream)
    {
        for(unsigned int i = 0; i < this->size(); i++)
            (*this)[i] = PiPo::Atom(initVal);
    };
    
    void clone(Attr *other) { *(dynamic_cast<std::vector<PiPo::Atom> *>(this)) = *(dynamic_cast<std::vector<PiPo::Atom> *>(other)); };
    
    unsigned int setSize(unsigned int size) { this->resize(size, PiPo::Atom(0)); return size; };
    unsigned int getSize(void) { return this->size(); }
    
    void set(unsigned int i, int val, bool silently = false)
    {
        if (i >= this->size())
            setSize(i + 1);
        
        (*this)[i] = PiPo::Atom(val);
        
        this->changed(silently);
    };
    
    void set(unsigned int i, double val, bool silently = false)
    {
        if (i >= this->size())
            setSize(i + 1);
        
        (*this)[i] = PiPo::Atom(val);
        
        this->changed(silently);
    };
    
    void set(unsigned int i, const char *val, bool silently = false)
    {
        if (i >= this->size())
            setSize(i + 1);
        
        (*this)[i] = PiPo::Atom(val);
        
        this->changed(silently);
    };
    
    int getInt(unsigned int i)
    {
        if(i >= this->size())
            i = this->size() - 1;
        
        return (*this)[i].getInt();
    };
    
    double getDbl(unsigned int i)
    {
        if(i >= this->size())
            i = this->size() - 1;
        
        return (*this)[i].getDouble();
    };
    
    const char *getStr(unsigned int i)
    {
        if(i >= this->size())
            i = this->size() - 1;
        
        return (*this)[i].getString();
    };
    
    PiPo::Atom *getPtr()	// return pointer to first data element
    {
        return &((*this)[0]);
    };
};


/** EMACS **
 * Local variables:
 * mode: c
 * c-basic-offset:2
 * End:
 */

#endif
