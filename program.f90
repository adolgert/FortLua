! This program uses a script to configure itself.

PROGRAM calculate
USE config

INTEGER :: argument_count
REAL*8 :: pressure
REAL*8 :: time
INTEGER :: step, stepcount
INTEGER :: status
REAL :: temperature_start
REAL, DIMENSION(1) :: pressure_args
INTEGER :: read_status
INTEGER :: cstatus
integer :: newval

character(50) :: string
character(:), allocatable :: astring

status = config_open('vals.lua')

call config_string('string',read_status, string)
astring = trim(string)

write(*,*) 'string = ', astring

temperature_start = config_real('temperature',read_status)
IF ( read_status .eq. 0 ) THEN
  WRITE (*,*) 'temperature = ', temperature_start
ELSE
  WRITE (*,*) 'error reading temperature'
ENDIF

newval = config_integer('newval',read_status)
IF ( read_status .eq. 0 ) THEN
  WRITE (*,*) 'newval = ', newval
ELSE
  WRITE (*,*) 'error reading newval'
ENDIF

stepcount=10

DO step = 1,stepcount
  time = step/1.0
  pressure_args(1) = time
  pressure=config_function('pressure',pressure_args,1,cstatus)
  WRITE (*,*) 'pressure = ', pressure
END DO


CALL config_close()

STOP
END PROGRAM calculate

