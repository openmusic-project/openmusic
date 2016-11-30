/**
 *
 * @file PiPoCollection.h
 * @author Norbert.Schnell@ircam.fr
 * @author joseph.larralde@ircam.fr
 * 
 * @brief PiPo Module Collection
 * 
 * Copyright (C) 2013 - 2015 by IMTR IRCAM – Centre Pompidou, Paris, France.
 * All rights reserved.
 * 
 */
#ifndef _PIPO_COLLECTION_
#define _PIPO_COLLECTION_

#include <map>
#include "PiPo.h"

//========================================//
//============== PiPo factory ============//
//========================================//

//------------------------------ PiPoCreatorBase
class PiPoCreatorBase
{
public:
    PiPoCreatorBase() {}
    virtual ~PiPoCreatorBase() {}
    virtual PiPo *create() = 0;
};

//------------------------------ PiPoCreator
template<class T>
class PiPoCreator : public PiPoCreatorBase
{
public:
    PiPoCreator() {}
    virtual ~PiPoCreator() {}
    virtual PiPo *create()
    {
        return new T(NULL);
    }
};

//------------------------------ PiPoCollection : interface with the outside world
//----------------------------------------------- uses the factory internally

/**
 * @brief PiPoCollection class : contains all base PiPo classes (with ability to hide
 * them from the outside world) and methods to register new PiPos and get PiPo chains
 *
 * \code
 *
 * #include "PiPoCollection.h"
 *
 * // example : register a new non native PiPo :
 * PiPoCollection::init();
 * PiPoCollection::addToCollection("mypipo", new PiPoCreator<MyPiPo>);
 * PiPo *mychain = PiPoCollection::create("slice:mypipo");
 *
 * \endcode
 *
 */

class PiPoCollection
{
public:
    static void init();
    static void addToCollection(std::string name, PiPoCreatorBase *creator);
    // static void addAlias(std::string alias, std::string name); ----> TODO
    static PiPo *create(std::string name);
private:
};

#endif
