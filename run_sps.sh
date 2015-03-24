#!/bin/bash
[[ "$1" == -v* ]] && set -x
#
[[ -d SHM ]]       || { echo "ERROR: directory SHM not found" ; exit 1 ;}
SHM_VAR=$(readlink -e SHM)
mkdir -p SHM/storage_model SHM/Data/Input/inrep
#
[[ -d Data ]]      || { echo "ERROR: directory Data not found" ; exit 1 ; }
[[ -d Data_disk ]] || { echo "ERROR: directory Data_disk not found" ; exit 1 ; }
#
[[ -f Data/Input/climato ]]        || { echo "INFO: copying climato" ; cp -f Data_disk/Input/climato Data/Input ; }
[[ -f Data/Input/Gem_geophy.fst ]] || { echo "INFO: copying Gem_geophy.fst" ; cp -f Data_disk/Input/Gem_geophy.fst Data/Input ; }
#
rm -f Data/Input/inrep/anal
ln -s ../anal Data/Input/inrep/anal
#
if [[ -d storage_model ]] ; then
  storage_model=$(readlink -e storage_model)
fi
export storage_model
#
[[ -r exper.cfg ]] || { echo "ERROR: cannot find $(pwd -P)/exper.cfg" ; exit 1 ; }
source ./exper.cfg
source functions_sps.dot
if [[ -d ${exper_archive}/${exper}.snapshot ]] ; then
  rsync -aruvxlH ${exper_archive}/${exper}.snapshot/. Data/.
  echo  "INFO: syncing run directory from ${exper_archive}/${exper}.snapshot"
fi
#
# make sure that there is a value for exper_current_date, exper_fold_date and storage_model in configuration file
#
[[ -z ${exper_current_date} ]] && exper_current_date=${exper_start_date} && echo "exper_current_date=${exper_current_date}" >>exper.cfg
#
[[ -z ${exper_fold_date} ]] && exper_fold_date="$(date -d${exper_end_date}+1year  +%Y%m%d)" && echo "exper_fold_date=${exper_fold_date}" >>./exper.cfg
#
[[ -d "${storage_model}" ]] || { echo "ERROR: ${storage_model} does not exist" ; exit 1 ; }
#
while true
do
  source ./exper.cfg
  #
  Extension=""
  ((exper_current_year>0)) && Extension="$(printf '_%3.3d' ${exper_current_year})"
  #
  if ((${exper_cycle_year:-999999}==0)) ; then
    echo "INFO: prescribed number of years of integration done"
    mkdir -p ${exper_archive}/${exper}.snapshot
    rsync -aruvxlH --delete Data/Input ${exper_archive}/${exper}.snapshot/.
    rm -rf Data/Input SHM/storage_model
    [[ "${SHM_VAR}" == /dev/shm* ]] && rm -rf ${SHM_VAR}
    exit 0
  fi
  #
  if [[ "${exper_current_date}" == "${exper_end_date}" ]] ; then
    echo "INFO: last date reached: ${exper_end_date}"
    mkdir -p ${exper_archive}/${exper}.snapshot
    rsync -aruvxlH --delete Data/Input ${exper_archive}/${exper}.snapshot/.
    rm -rf Data/Input SHM/storage_model
    [[ "${SHM_VAR}" == /dev/shm* ]] && rm -rf ${SHM_VAR}
    exit 0
  fi
  #
  set -x
  if [[ -f Data/Input/anal_${exper_fold_date} ]] ; then
    u.re_tag_date Data/Input/anal_${exper_fold_date} Data/Input/anal_${exper_start_date} $(r.date ${exper_start_date})
    update_cfg exper.cfg exper_current_date ${exper_start_date}
  fi
  set +x
  #
  pre_sps.sh  || { echo "ERROR: pre_sps failed" ; exit 1 ; }
  #
  echo "INFO: sps.ksh ${exper_cpu_config}"
  mkdir -p   ${exper_archive}/${exper}/Listings
  sps.ksh ${exper_cpu_config} >${exper_archive}/${exper}/Listings/sps_${exper_current_date:-${exper_start_date}}${Extension}.lst 2>&1 \
    || sps.ksh ${exper_cpu_config2} >${exper_archive}/${exper}/Listings/sps_${exper_current_date:-${exper_start_date}}${Extension}.lst.2 2>&1 \
    || { echo "ERROR: sps.ksh failed" ; exit 1 ; }
  gzip -9 ${exper_archive}/${exper}/Listings/sps_${exper_current_date:-${exper_start_date}}${Extension}.lst*  &
  #
  post_sps.sh  || { echo "ERROR: post_sps failed" ; exit 1 ; }
  wait
  #
  source ./exper.cfg
  [[ "${exper_current_date}" != "${exper_fold_date}" ]] && rm -f Data/Input/anal_${exper_fold_date}
  #
done
