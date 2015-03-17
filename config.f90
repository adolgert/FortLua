!> This module loads a Lua file to read values and execute functions.

MODULE config
  ! The USE statement comes first
  USE iso_c_binding, only: C_CHAR, C_NULL_CHAR, C_INT, C_PTR, &
                           C_FUNPTR, C_DOUBLE
  IMPLICIT NONE
  PRIVATE mluastate !< This is an opaque pointer to the Lua interpreter.

  ! Module scope variables
  ! The lua_State pointer is stored opaquely in Fortran in this
  ! module-level variable.
  TYPE(c_ptr) :: mluastate
  !INTEGER(c_int) :: LUA_GLOBALSINDEX = -10002
  INTEGER(c_int) :: LUA_IDSIZE = 60

  INTEGER(c_int) :: LUA_TNIL = 0
  INTEGER(c_int) :: LUA_TBOOLEAN = 1
  INTEGER(c_int) :: LUA_TLIGHTUSERDATA = 2
  INTEGER(c_int) :: LUA_TNUMBER = 3
  INTEGER(c_int) :: LUA_TSTRING = 4
  INTEGER(c_int) :: LUA_TTABLE = 5
  INTEGER(c_int) :: LUA_TFUNCTION = 6
  INTEGER(c_int) :: LUA_TUSERDATA = 7
  INTEGER(c_int) :: LUA_TTHREAD = 8


  INTERFACE
    ! This interface is a subset of the Lua interface, but you
    ! get the point.
    !
    ! This uses some of Fortran's ability to bind directly to C.
    ! The value option tells Fortran not to pass the argument
    ! using a pointer to the argument.
    ! For strings, it looks like the proper declaration is
    ! an array of CHARACTER(KIND=c_char) but that Fortran will
    ! happily translate CHARACTER(KIND=c_char,LEN=*) to the 
    ! array of single chars.


    FUNCTION luaL_newstate() bind(C,name="luaL_newstate")
      USE iso_c_binding, only: c_ptr, c_funptr
      TYPE(c_ptr) :: luaL_newstate
    END FUNCTION luaL_newstate

    SUBROUTINE lua_close(lstate) bind(C,name="lua_close")
      USE iso_c_binding, only: c_ptr
      TYPE(c_ptr), value :: lstate
    END SUBROUTINE lua_close

    SUBROUTINE luaL_openlibs(lstate) bind(C,name="luaL_openlibs")
      USE iso_c_binding, only: c_ptr
      TYPE(c_ptr), value :: lstate
    END SUBROUTINE luaL_openlibs

    function luaL_loadfilex(L, filename, mode) bind(c, name="luaL_loadfilex")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: filename
      character(kind=c_char), dimension(*) :: mode
      integer(kind=c_int) :: luaL_loadfilex
    end function luaL_loadfilex

    FUNCTION lua_gettop(lstate) bind(C, name="lua_gettop")
      USE iso_c_binding, only: c_int, c_ptr
      INTEGER(c_int) :: lua_gettop
      TYPE(c_ptr), value :: lstate
    END FUNCTION lua_gettop

    FUNCTION lua_type(lstate, stackIdx) bind(C,name="lua_type")
      USE iso_c_binding, only: c_int, c_ptr
      INTEGER(c_int) :: lua_type
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_type

    FUNCTION lua_checkstack(lstate, stackIdx) bind(C,name="lua_checkstack")
      USE iso_c_binding, only: c_int, c_ptr
      INTEGER(c_int) :: lua_checkstack
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_checkstack

    subroutine lua_getglobal(L, k) bind(c, name="lua_getglobal")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_getglobal

    !> Set the top of the stack.
    !! lua_pop is defined as lua_settop(L,-(n)-1) in a macro for C.
    SUBROUTINE lua_settop(lstate,stackIdx) bind(C,name="lua_settop")
      USE iso_c_binding, only: c_ptr, c_int
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END SUBROUTINE lua_settop

    SUBROUTINE lua_pushnumber(lstate,setval) bind(C,name="lua_pushnumber")
      USE iso_c_binding, only: c_ptr, c_double
      TYPE(c_ptr), value :: lstate
      REAL(c_double), value :: setval
    END SUBROUTINE lua_pushnumber

    function lua_pcallk(L, nargs, nresults, errfunc, ctx, k) bind(c, name="lua_pcallk")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: nargs
      integer(kind=c_int), value :: nresults
      integer(kind=c_int), value :: errfunc
      integer(kind=c_int), value :: ctx
      type(c_ptr), value :: k
      integer(kind=c_int) :: lua_pcallk
    end function lua_pcallk

    FUNCTION lua_isfunction(lstate,stackIdx) bind(C,name="lua_isfunction")
      USE iso_c_binding, only: c_ptr, c_int
      INTEGER(c_int) :: lua_isfunction
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_isfunction

    FUNCTION lua_isnumber(lstate,stackIdx) bind(C,name="lua_isnumber")
      USE iso_c_binding, only: c_ptr, c_int
      INTEGER(c_int) :: lua_isnumber
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_isnumber

    function lua_tonumberx(L, index, isnum) bind(c, name="lua_tonumberx")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: isnum
      real(kind=c_double) :: lua_tonumberx
    end function lua_tonumberx

    FUNCTION lua_tointeger(lstate,stackIdx) bind(C,name="lua_tointeger")
      USE iso_c_binding, only: c_ptr, c_int, c_size_t
      REAL(c_size_t) :: lua_tointeger
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_tointeger

  END INTERFACE

  CONTAINS

    function luaL_loadfile(lstate, filename) result(errcode)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      character(len=*) :: filename
      integer :: errcode

      character(len=len_trim(filename)+1) :: c_filename
      character(len=3) :: c_mode
      integer(kind=c_int) :: c_errcode

      c_filename = trim(filename) // c_null_char
      c_mode = "bt" // c_null_char
      c_errcode = luaL_loadfilex(lstate, c_filename, c_mode)
      errcode = c_errcode
    end function luaL_loadfile

    function lua_pcall(lstate, nargs, nresults, errfunc) result(errcode)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      integer :: nargs
      integer :: nresults
      integer :: errfunc
      integer :: errcode

      integer(kind=c_int) :: c_nargs
      integer(kind=c_int) :: c_nresults
      integer(kind=c_int) :: c_errfunc
      integer(kind=c_int) :: c_errcode

      c_nargs = nargs
      c_nresults = nresults
      c_errfunc = errfunc

      c_errcode = lua_pcallk(lstate, c_nargs, c_nresults, c_errfunc, &
        &                    0_c_int, C_NULL_PTR)
      errcode = c_errcode
    end function lua_pcall

    function lua_tonumber(lstate, index) result(number)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      integer :: index
      real :: number

      integer(kind=c_int) :: c_index
      integer(kind=c_int) :: isnum

      c_index = index
      number = real(lua_tonumberx(lstate, c_index, isnum), &
        &           kind=kind(number))
    end function lua_tonumber


  !> Open a Lua configuration file by name.
  !! The state of the Lua file is held in the module and must be
  !! closed when you are done.
  INTEGER FUNCTION config_open(fname)
    CHARACTER(LEN=*) :: fname
    INTEGER(c_int) :: filesuccess, callsuccess

    mluastate=luaL_newstate()
    CALL luaL_openlibs(mluastate)

    print *, 'filesuccess'
    filesuccess = luaL_loadfile(mluastate, TRIM(fname)//C_NULL_CHAR)
    print *, 'filesuccess', filesuccess
    IF ( filesuccess .eq. 0 ) THEN
      callsuccess = lua_pcall(mluastate,0,0,0)
      print *, 'callsuccess', callsuccess
      IF ( callsuccess .eq. 0 ) THEN
        ! This is equivalent to the macro lua_pop.
        CALL lua_settop(mluastate,-2)
        config_open=1
        print *, 'callsuccess2', callsuccess
      ELSE
         config_open=0
      ENDIF
    ELSE
      config_open=0
    ENDIF

  END FUNCTION config_open



  !> Close the Lua configuration, which releases the interpreter.
  SUBROUTINE config_close
    call lua_close(mluastate)
  END SUBROUTINE config_close



  !> Retrieve the value of a floating point variable.
  FUNCTION config_real(name,status)
    REAL :: config_real
    CHARACTER(LEN=*) :: name
    INTEGER :: status
    INTEGER(c_int) :: stackstart

    ! We compare the stack before and after our work to discover
    ! whether we have corrupted it. Otherwise debugging errors
    ! can be difficult.
    stackstart = lua_gettop(mluastate)

    print *, 'post gettop', name
    !DBG CALL lua_getfield(mluastate,LUA_GLOBALSINDEX,TRIM(name)//C_NULL_CHAR)
    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)
    

    print *, 'post getfield'
    IF ( lua_isnumber(mluastate,-1) .NE. 0 ) THEN
      config_real=lua_tonumber(mluastate,-1)
      ! This is the same as Lua pop 1.
      CALL lua_settop(mluastate,-2)
      status = 0
    ELSE
      config_real=0
      status = -1
    ENDIF
    IF (stackstart .ne. lua_gettop(mluastate)) THEN
       WRITE(*,*) 'The stack is a different size coming out of config_real'
    ENDIF

  END FUNCTION config_real



  !> Retrieve the value of an integer variable.
  FUNCTION config_integer(name,status)
    INTEGER :: config_integer
    CHARACTER(LEN=*) :: name
    INTEGER :: status
    INTEGER(c_int) :: stackstart

    stackstart = lua_gettop(mluastate)

    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)

    IF ( lua_isnumber(mluastate,-1) .NE. 0 ) THEN
      config_integer=lua_tonumber(mluastate,-1)
      ! This is the same as Lua pop 1.
      CALL lua_settop(mluastate,-2)
      status = 0
    ELSE
      config_integer=0
      status = -1
    ENDIF
    IF (stackstart .ne. lua_gettop(mluastate)) THEN
       WRITE(*,*) 'The stack is a different size coming out of config_integer'
    ENDIF

  END FUNCTION config_integer




  !> Evaluate a function in the config file and get its result.
  FUNCTION config_function(name,args,nargs,status)
    REAL :: config_function
    CHARACTER(LEN=*) :: name
    REAL, DIMENSION(*) :: args
    REAL(KIND=c_double) :: anarg
    INTEGER :: nargs
    INTEGER :: status
    INTEGER :: iargs
    INTEGER(c_int) :: stackstart

    stackstart = lua_gettop(mluastate)

    config_function = 0


    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)
    IF ( lua_type(mluastate,-1) .eq. LUA_TFUNCTION ) THEN
        DO iargs = 1,nargs
          anarg = args(iargs)
          CALL lua_pushnumber(mluastate,anarg)
        ENDDO
        IF (lua_pcall(mluastate,nargs,1,0) .eq. 0) THEN
          if (lua_isnumber(mluastate,-1) .ne. 0) THEN
            config_function = lua_tonumber(mluastate,-1)
            CALL lua_settop(mluastate,-2)
          ELSE
            ! Nothing to pop here
            status=-3
          ENDIF
        ELSE
          CALL lua_settop(mluastate,-2)
          status=-2
        ENDIF
    ELSE
        CALL lua_settop(mluastate,-2)
        status=-1
    ENDIF
    IF (stackstart .ne. lua_gettop(mluastate)) THEN
       WRITE(*,*) 'The stack is a different size coming out of config_function'
    ENDIF

  END FUNCTION config_function


END MODULE config
