#!/bin/bash
#
while [[ $# -gt 0 ]] ; do
   case $1 in
      (-v|--verbose)        VeRbOsE="-x"   ;;   
      (-d|--debug)          SPS_DEBUG="yes" ;;
      (-gdb|--gdb)          export RUN_IN_PARALLEL_EXTRAS="${RUN_IN_PARALLEL_EXTRAS} -preexec gdb"       ; SPS_DEBUG="yes" ;;
      (-preexec|--preexec)  export RUN_IN_PARALLEL_EXTRAS="${RUN_IN_PARALLEL_EXTRAS} -preexec ${2:-gdb}" ; SPS_DEBUG="yes" ; shift ;;
      (-*) echo "Error: unknown option '$1'"; exit 1;;
   esac
   previous=$1
   shift
done
set ${VeRbOsE:-+x}
#
rm -f SHM
if tty -s ; then    # interactive case, raise default memory limits
  ulimit -s unlimited
  ulimit -d unlimited
  ulimit -d 8000000
  mkdir -p /dev/shm/${USER}
  export RAMDISK=/dev/shm/${USER}   # RAMDISK in defined for batch jobs on guillimin
fi
[[ -d ${RAMDISK} ]] && ln -s ${RAMDISK} SHM
#
unset FatalError
((FatalError=0))
[[ -r configexp.cfg ]] || { echo "ERROR: cannot find configexp.cfg" ; ((FatalError=FatalError+1)) ; }
[[ -r sps.cfg ]]       || { echo "ERROR: cannot find sps.cfg"       ; ((FatalError=FatalError+1)) ; }
[[ -r sps.dict ]]      || { echo "ERROR: cannot find sps.dict"      ; ((FatalError=FatalError+1)) ; }
[[ -r outcfg.out ]]    || { echo "ERROR: cannot find outcfg.out"    ; ((FatalError=FatalError+1)) ; }
#
source ./configexp.cfg
[[ -d sps_Linux_x86-64.Abs ]] && echo "ERROR: expecting file for sps_Linux_x86-64.Abs"              && ((FatalError=FatalError+1))
[[ -x sps_Linux_x86-64.Abs ]] || ln -sf "${exper_abs:-/dev/null}" sps_Linux_x86-64.Abs
[[ -x sps_Linux_x86-64.Abs ]] || { echo "ERROR: cannot find executable sps_Linux_x86-64.Abs"         ; ((FatalError=FatalError+1)) ; }
[[ -r ${SPS_phy_intable} ]]   || { echo "ERROR: cannot find ${SPS_phy_intable:-physics_input_table}" ; ((FatalError=FatalError+1)) ; }
[[ -r ${SPS_dyn_intable} ]]   || { echo "ERROR: cannot find ${SPS_dyn_intable:-dyn_input_table}"     ; ((FatalError=FatalError+1)) ; }
[[ -d ${exper_archive} ]]     || { echo "ERROR: archival directory not found"                        ; ((FatalError=FatalError+1)) ; }
[[ -d ${exper_archive} ]]     && ln -s ${exper_archive} ArchiveDirectory
#
[[ -L SHM && -d SHM ]]        || { echo "ERROR: SHM must be a soft link to an existing directory"    ; ((FatalError=FatalError+1)) ;}
#
# create link to OUT
#
rm -f OUT
[[ -d OUT ]] && { echo "ERROR: OUT is an existing directory and should not"                          ; ((FatalError=FatalError+1)) ;}
ln -s __workdir__Linux_x86-64/output/cfg_0000 OUT
#
rm -f storage_model
[[ -d storage_model ]] && { echo "ERROR: storage_model is an existing directory and should not"      ; ((FatalError=FatalError+1)) ;}
ln -s SHM/storage_model storage_model
#
# make sure include is linked to current directory
#
rm -f include
[[ -d include ]] && { echo "ERROR: include is an existing directory and should not"                  ; ((FatalError=FatalError+1)) ;}
ln -s . include
#
# make sure SPS_cfgs/cfg_0000 points to surrent directory
#
mkdir -p SPS_cfgs
rm -f SPS_cfgs/cfg_0000
[[ -d SPS_cfgs/cfg_0000 ]] && { echo "ERROR: SPS_cfgs/cfg_0000 is an existing directory and should not" ; ((FatalError=FatalError+1)) ;}
ln -s $(pwd -P) SPS_cfgs/cfg_0000
#
SHM_VAR=$(readlink -e SHM)
[[ -L SHM && -d SHM/ ]] && mkdir -p SHM/storage_model SHM/Data/Input/inrep
[[ -d SHM/storage_model ]]    ||  { echo "ERROR: SHM/storage_model directory not found"                ; ((FatalError=FatalError+1)) ; }
[[ -d SHM/Data/Input/inrep ]] ||  { echo "ERROR: SHM/Data/Input/inrep directory not found"             ; ((FatalError=FatalError+1)) ; }
#
rm -f Data
[[ -d Data ]] && { echo "ERROR: Data is an existing directory and should not"                          ; ((FatalError=FatalError+1)) ;}
ln -s SHM/Data Data
#
[[ -d Data ]]      || { echo "ERROR: directory Data not found"                                         ; ((FatalError=FatalError+1)) ; }
#[[ -d Data_disk ]] || { echo "ERROR: directory Data_disk not found" ; exit 1 ; }
#
((FatalError>0)) && exit 1
#
[[ -f Data/Input/climato ]]        || { echo "INFO: copying climato" ; cp -f ${exper_climato:-DoesNotExist} Data/Input/climato ; }
[[ -f Data/Input/climato ]]        || { echo "ERROR: climatology file not found" ; exit 1 ; }
[[ -f Data/Input/Gem_geophy.fst ]] || { echo "INFO: copying Gem_geophy.fst" ; cp -f ${exper_geophy:-DoesNotExist} Data/Input/Gem_geophy.fst ; }
[[ -f Data/Input/Gem_geophy.fst ]] || { echo "ERROR: geophysical fields file not found" ; exit 1 ; }
#
rm -f Data/Input/inrep/anal
ln -s ../anal Data/Input/inrep/anal
#
if [[ -d storage_model ]] ; then
  storage_model=$(readlink -e storage_model)
fi
export storage_model
#
source functions_sps.dot
#
if [[ -d ${exper_archive}/${exper}.snapshot ]] ; then   # there is a snapshot, use it
  rsync -aruvxlH ${exper_archive}/${exper}.snapshot/. Data/.
  echo  "INFO: syncing run directory from ${exper_archive}/${exper}.snapshot"
fi
#
# make sure that there is a value for exper_current_date, and exper_fold_date in configuration file
#
[[ -z ${exper_current_date} ]] && exper_current_date=${exper_start_date} && echo "exper_current_date=${exper_current_date}" >>configexp.cfg
#
[[ -z ${exper_fold_date} ]] && exper_fold_date="$(date -d${exper_end_date}+1year  +%Y%m%d)" && echo "exper_fold_date=${exper_fold_date}" >>./configexp.cfg
#
[[ -d "${storage_model}" ]] || { echo "ERROR: directory ${storage_model} does not exist" ; exit 1 ; }   # environment variable storage_model is necessary
#
# loop over months, until end date reached or specific number of integration years done
#
while true
do
  source ./configexp.cfg    # get updated values from ./configexp.cfg
  #
  Extension=""   # Extension is used only when exper_current_year > 0
  ((exper_current_year>0)) && Extension="$(printf '_%3.3d' ${exper_current_year})"
  #
  if ((${exper_cycle_year:-999999}==0)) ; then             # requested number of integration years done
    echo "INFO: prescribed number of years of integration done"
    cleanup_dirs
    exit 0
  fi
  #
  if ((exper_current_date>exper_end_date)) ; then          # end date reached
    echo "INFO: last date reached: ${exper_end_date}"      # exper_current_date is updated by post_sps.sh
    cleanup_dirs
    exit 0
  fi
  #
  if ((exper_current_date==exper_end_date)) ; then         # end date really reached ?
    if [[ -z ${extra_time} && -z ${extra_steps} && -z ${extra_hours} ]] ; then   # if extra_time is present, we are not done yet
      echo "INFO: last date reached: ${exper_end_date}"
      cleanup_dirs
      exit 0
    fi
  fi
  #
  set -x
  if [[ -f Data/Input/anal_${exper_fold_date} ]] ; then     # fold date reached, reset dates to exper_start_date
    u.re_tag_date Data/Input/anal_${exper_fold_date} Data/Input/anal_${exper_start_date} $(r.date ${exper_start_date})
    update_cfg configexp.cfg exper_current_date ${exper_start_date}
  fi
  set +x
  #####################################################    PRE    #############################################################
  pre_sps.sh  || { echo "ERROR: pre_sps failed" ; exit 1 ; }
  #####################################################    SPS    #############################################################
  echo "INFO: sps.ksh ${exper_cpu_config}"
  mkdir -p   ${exper_archive}/${exper}/Listings   # for sps listings
  [[ -d ${exper_archive}/${exper}/Listings ]] ||  { echo "ERROR: cannot create  ${exper_archive}/${exper}/Listings" ; exit 1 ; }
  export sps=$(pwd -P)
  if [[ -n ${SPS_DEBUG} ]] ; then
    sps.ksh ${exper_cpu_config}  ||  { echo "ERROR: sps.ksh failed" ; exit 1 ; }
  else
    sps.ksh ${exper_cpu_config} >${exper_archive}/${exper}/Listings/sps_${exper_current_date:-${exper_start_date}}${Extension}.lst 2>&1 \
      || sps.ksh ${exper_cpu_config2} >${exper_archive}/${exper}/Listings/sps_${exper_current_date:-${exper_start_date}}${Extension}.lst.2 2>&1 \
      || { echo "ERROR: sps.ksh failed" ; exit 1 ; }
    gzip -9 ${exper_archive}/${exper}/Listings/sps_${exper_current_date:-${exper_start_date}}${Extension}.lst*  &
  fi
  #####################################################    POST   #############################################################
  post_sps.sh  || { echo "ERROR: post_sps failed" ; exit 1 ; }
  wait
  #####################################################    DONE   #############################################################
  source ./configexp.cfg
  [[ "${exper_current_date}" != "${exper_fold_date}" ]] && rm -f Data/Input/anal_${exper_fold_date}     # remove unneeded file
  #
done
