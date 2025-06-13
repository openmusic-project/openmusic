#!/bin/bash

#==============================================================================
# @brief    This script prepares an OM package
# @author   Karim Haddad
#==============================================================================

OM_VERSION="8.0"

VERSION="8.0"
REVISION="1"
ARCH="amd64"
DEB_VERSION=${VERSION}-${REVISION}_${ARCH}

#Changer test apres...
BUILD_DIR=/tmp/OM/BUILD


# path to the compiled folder :
#BIN_PATH=${BUILD_DIR}/openmusic

# path to the LW compiled folder :
BIN_PATH=${BUILD_DIR}/'OM_'${OM_VERSION}


# path to the debian package folder to package :
DEB_DIR=${BUILD_DIR}/'openmusic_'${DEB_VERSION}

# current date
DATE=`date +%d-%m-%Y`

#FINAL_DPKG=${BUILD_DIR}/distrib/linux/OM_${OM_VERSION}-debian-${DATE}.dpkg

#==============================================================================
CYAN='\033[0;36m'
BLUE='\033[0;34m'
RED='\033[0;31m'
GREEN='\033[92m'
NC='\033[0m' # No Color

# store current folder
CURRENT_PWD=`pwd`

#copy control file
#cp control ${BUILD_DIR}

# create necessary folders 
mkdir ${DEB_DIR}
mkdir ${DEB_DIR}/usr
mkdir ${DEB_DIR}/usr/share
mkdir ${DEB_DIR}/usr/share/applications
mkdir ${DEB_DIR}/usr/share/openmusic
mkdir ${DEB_DIR}/usr/share/fonts
mkdir ${DEB_DIR}/usr/share/fonts/omfonts
mkdir ${DEB_DIR}/usr/share/pixmaps
mkdir ${DEB_DIR}/usr/bin


# copy binaries to folders
cp ${BIN_PATH}/OM_8.0 ${DEB_DIR}/usr/bin/
cp -r ${BIN_PATH}/build/linux ${DEB_DIR}/usr/share/openmusic
cp ${BIN_PATH}/build/linux/OM.desktop ${DEB_DIR}/usr/share/applications
cp ${BIN_PATH}/build/linux/omlogo.png ${DEB_DIR}/usr/share/pixmaps
cp -r ${BIN_PATH}/code ${DEB_DIR}/usr/share/openmusic
cp -r ${BIN_PATH}/init ${DEB_DIR}/usr/share/openmusic
cp -r ${BIN_PATH}/resources ${DEB_DIR}/usr/share/openmusic
cp -r ${BIN_PATH}/build ${DEB_DIR}/usr/share/openmusic
cp  ${BIN_PATH}/README.LINUX.md ${DEB_DIR}/usr/share/openmusic
cp  ${BIN_PATH}/NEWS ${DEB_DIR}/usr/share/openmusic
cp  ${BIN_PATH}/LICENSE ${DEB_DIR}/usr/share/openmusic
cp -r ${BIN_PATH}/resources/fonts/linux/*.ttf ${DEB_DIR}/usr/share/fonts/omfonts


#remove packaging files:
#rm ${BIN_PATH}/NEWS ${DEB_DIR}/usr/share/openmusic/build/linux/om_prepare_deb_final.sh
#rm ${BIN_PATH}/NEWS ${DEB_DIR}/usr/share/openmusic/build/linux/control
rm ${DEB_DIR}/usr/share/openmusic/build/linux/om_prepare_deb_final.sh
rm ${DEB_DIR}/usr/share/openmusic/build/linux/control
#mkdir ${BUILD_DIR}/distrib/linux

#create metadata folder and file
mkdir ${DEB_DIR}/DEBIAN
cp "control" ${DEB_DIR}/DEBIAN


#Change all the folder permissions to root:
#chown root:root -R ${BUILD_DIR}/'OM_'${OM_VERSION}

#Make a symbolic link to image
ln -r -s ${DEB_DIR}/usr/bin/OM_8.0 ${DEB_DIR}/usr/bin/openmusic


#It's here that fucks up!!!!
#Change all the folder permissions to root:
#chown root:root -R ${BUILD_DIR}/'OM_'${OM_VERSION}

#Change the script's permissions:
chmod 0755 ${DEB_DIR}/usr/bin/OM_8.0
chmod 0755 ${DEB_DIR}/usr/bin/openmusic



#Finally, you can run: 
dpkg -b ${DEB_DIR}


#Remove build working folder
#a tester
rm -fr ${DEB_DIR}

#==============================================================================
# Code-sign the thing
echo " "
echo -e "${CYAN}Build Done .... ${SIGNED_APP_PATH}...${NC}"

# return to current folder
cd ${CURRENT_PWD}
