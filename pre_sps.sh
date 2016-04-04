#!/bin/bash
#
# Data/ sould be pointing to where input data is expected for this month
# Data/Input:
#  climato             (climatology file)
#  Gem_geophy.fst      (geophysical fields)
#  anal                (initial conditions for this month)
# Data/Input/inrep:
#  anal (link to ../anal)
#  driving data for the month (names ending in _YYYYMM)
#
# Data/Input might contain anal_YYYYMM left by a previous post_sps.sh
#
# local file configexp.cfg contains the configuration for this experiment
#
[[ -r configexp.cfg ]] || { echo "ERROR: cannot find $(pwd -P)/configexp.cfg" ; exit 1 ; }
source ./configexp.cfg
#
[[ -n "${exper_current_date}" ]] || { echo "ERROR: exper_current_date not set" ; exit 1 ; }
#
# extension is normally used when running the same driving data over and over (perpetual year or the like)
# but it can also be used for normal multi year integrations
#
Extension=""
((exper_current_year>0)) && Extension="$(printf '_%3.3d' ${exper_current_year})"
#
Delta=${exper_delta:-1month}
#
StepStartDate=${exper_current_date}
StepEndDate="$(date -d${StepStartDate}+${Delta} +%Y%m%d)"
if ((StepEndDate>exper_end_date)) ; then  # run for last month may be shorter than a month
  extra_steps=${extra_steps:-0}           # extra steps for last day
  extra_hours=${extra_hours:-0}           # extra hours for last day  (will override extra_steps if specified)
  extra_time=${extra_time:-00:00:00}
  StepEndDate=${exper_end_date}
else
  extra_steps=0
  extra_hours=0
  extra_time='00:00:00'
fi
echo "INFO: running from ${StepStartDate} to ${StepEndDate}, (${Delta})"
#
# get initial conditions files for this month (better be available or else !!)
#   ${exper_anal1} ${exper_anal2} are the 2 possible sources
#   filenames expected to be anal_*_YYYYMMDD${Extension}  (no more than ONE match for a given month)
#
[[ -d Data/Input ]] || { echo "ERROR: directory Data/Input does not exist" ; exit 1 ; }
#
rm -f Data/Input/anal     # get rid of old initial conditions file
((exper_current_year<=1)) && \
  [[ -f "${exper_anal0}" ]] && \
  [[ ${exper_anal0##*/} == anal_*_${StepStartDate} ]] && \
  cp ${exper_anal0} Data/Input/anal_${StepStartDate} &&
  echo "INFO: copying ${exper_anal0} as Data/Input/anal_${StepStartDate}"
#
if [[ -f Data/Input/anal_${StepStartDate} ]] ; then   # normally put there by post_sps or run_sps
  mv Data/Input/anal_${StepStartDate} Data/Input/anal
  echo "INFO: using Data/Input/anal_${StepStartDate} as initial conditions"
else
  for Target in ${exper_anal1} ${exper_anal2}         # go fishing into directories containing initial conditions
  do
    echo "INFO: looking for ${Target}/anal_*_${StepStartDate}${Extension}"
    for Target2 in ${Target}/anal_*_${StepStartDate}${Extension}
    do
      if [[ -f ${Target2} ]] ; then
        echo "INFO: using ${Target2} as initial conditions" && \
        cp ${Target2} Data/Input/anal && \
        break 2
      fi
    done
  done
fi
[[ -f Data/Input/anal ]] || { echo "ERROR: cound not find initial conditions file for ${StepStartDate}" ; exit 1 ; }
#
# get driving data files for this month (better be available or else !!)
# files from directory ${exper_depot1} or ${exper_depot2}, names expected to end with _YYYYMM
#
#ls -l ${exper_depot1}/*_${StepStartDate%??} ${exper_depot2}/*_${StepStartDate%??}
Nfiles=0
for Target in $(ls -1 ${exper_depot1}/*_${StepStartDate%??} ${exper_depot2}/*_${StepStartDate%??} )
do
  Target2=${Target##*/}
  if [[ -r Data/Input/inrep/${Target2} ]] ; then
    echo "INFO: ${Target2} found in Data/Input/inrep/"
  else
    cp ${Target} Data/Input/inrep/.     # file not already there,  copy it
    echo "INFO: using ${Target}"
  fi
  ((Nfiles=Nfiles+1))
done
if ((Nfiles == 0));then
  echo "ERROR: no driving data found for ${StepStartDate%??}"
  exit 1
fi
#ls -l Data/Input/inrep/*_${StepStartDate%??}
#
# get driving data files for next month (if available and not already there)
# (same rules as driving data for this month)
#
#ls -l ${exper_depot2}/*_${StepEndDate%??} ${exper_depot2}/*_${StepEndDate%??}
for Target in $(ls -1 ${exper_depot1}/*_${StepEndDate%??} ${exper_depot2}/*_${StepEndDate%??} )
do
  Target2=${Target##*/}
  if [[ -r Data/Input/inrep/${Target2} ]] ; then
    echo "INFO: ${Target2} found in Data/Input/inrep/"
  else
    cp ${Target} Data/Input/inrep/.    # file not already there,  copy it
    echo "INFO: using ${Target}"
  fi
done
#ls -l Data/Input/inrep/*_${StepStartDate%??}
#
# calculate length of integration in hours and time steps (how many in this month)
# Date1  start of integration
# Date2  end of integration  (usually 1 month later but might be less)
#
Date1=$(date -d"${StepStartDate} 00:00:00 GMT" +%s)
Date2=$(date -d"${StepEndDate} ${extra_time} GMT" +%s)
TimeStep=${exper_deltat:-10800}   # default 3 hour timestep
Nsteps=$(((Date2-Date1)/TimeStep))
((extra_hours>0)) && ((extra_steps=extra_hours*3600/TimeStep))   # if extra_hours > 0, compute extra_steps to override Nsteps
((extra_steps>0)) && ((Nsteps=Nsteps+extra_steps))    # if extra_steps > 0, override Nsteps (always = 0  if not last month)
((Nhours=Nsteps*TimeStep/3600))                       # extra_steps/extra_hours are used if run for last month is not a full month
echo INFO: performing ${Nsteps} timesteps in ${Nhours} hours integration   file=pm${DaTe}000000-??-??_000000h
#
# fix Step_runstrt_S and Step_total in sps.cfg
#
#find . -mindepth 3 -maxdepth 3 -name sps.cfg -exec cp {} sps.cfg_old \;
#sed -e "s/Step_runstrt_S =[^.]*/Step_runstrt_S = '${StepStartDate}/" -e "s/Step_total.*/Step_total = ${Nsteps}/" <sps.cfg_old >sps.cfg_new
sed -e "s/Step_runstrt_S =[^.]*/Step_runstrt_S = '${StepStartDate}/" \
    -e "s/Step_dt.*/Step_dt = ${TimeStep}./"  \
    -e "s/Step_total.*/Step_total = ${Nsteps}/" <sps.cfg >sps.cfg_new  && \
  mv sps.cfg_new sps.cfg
[[ -f outcfg.out.orig ]] && mv outcfg.out.orig outcfg.out
grep -q MONTHLY outcfg.out && \
   cp outcfg.out outcfg.out.orig && \
   sed -e "s/MONTHLY/${Nhours}/" <outcfg.out.orig >outcfg.out
true
#find . -mindepth 3 -maxdepth 3 -name sps.cfg -exec mv sps.cfg_new {} \;
#rm -f sps.cfg_old
#exit 0
#
