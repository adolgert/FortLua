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

status = config_open('vals.lua')


temperature_start = config_real('temperature',read_status)
IF ( read_status .eq. 0 ) THEN
  WRITE (*,*) 'temperature = ', temperature_start
ELSE
  WRITE (*,*) 'error reading temperature'
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

