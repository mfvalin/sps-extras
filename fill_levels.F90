program fill_levels
  use ISO_C_BINDING
  implicit none
  include 'convert_ip123.inc'
  integer, parameter :: MAX_NAMES=16
  integer :: levels, nvars
  character(len=4096) :: filename
  character(len=4), dimension(MAX_NAMES) :: names
  integer :: i, status, length
  character(len=16) :: string

  filename = 'DoesNotExistYet'
  names = "    "
  levels = 0

  CALL GET_COMMAND_ARGUMENT(1 , filename, length, status)
  if(status .ne. 0) goto 999
  print 100,"INFO: processing '"//trim(filename)//"'"

  CALL GET_COMMAND_ARGUMENT(2 , string, length, status)
  if(status .ne. 0) goto 999

  read(string,*,iostat=status) levels
  if(status .ne. 0) goto 999
  print 100,"INFO: number of levels =",levels

  CALL GET_COMMAND_ARGUMENT(3 , string, length, status)
  if(status .ne. 0) goto 999

  read(string,*,iostat=status) nvars
  if(status .ne. 0) goto 999
  print 100,"INFO: number of variables =",nvars

  do i = 1, nvars
    CALL GET_COMMAND_ARGUMENT(i+3 , names(i), length, status)
    if(status .ne. 0) goto 999
    print 100,"INFO: variable",i," = '"//names(i)//"'"
  enddo
  stop
  100 format(A,I4,1X,A)
999 continue
  print *,"ERROR in arguments, aborting run"
  stop
end
