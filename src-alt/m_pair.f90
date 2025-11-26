
module m_pair

  use iso_c_binding, only : c_ptr
  use m_objects, only : c_object, objects, ivalue
  implicit none

  ! Type pair for (key, value)
  type pair
    character(:), allocatable :: key
    class(c_object), allocatable :: object
   contains
    procedure :: getkey
    procedure :: getval
    !procedure :: set !! Set done by the constructor
  end type pair

  interface pair
    procedure :: pair_rk0_constructor
    procedure :: pair_rk1_constructor
  end interface pair

  interface assignment(=)
    module procedure :: assign
  endinterface


 CONTAINS


  !...................................................................
  type(pair) function pair_rk0_constructor( key, value )result( this )
    character(*), intent( in ) :: key
    class(*),     intent( in ) :: value

    this% key = key
    select type( value )
      type is( integer )
        !this% object = objects( value )
        this% object = ivalue( value )
      class default
        !ERROR( "Value kind is not taking into account" )
        print*, "Value kind is not taking into account"
    end select

  end function pair_rk0_constructor

  !...................................................................
  type(pair) function pair_rk1_constructor( key, value )result( this )
    character(*), intent( in ) :: key
    class(*),     intent( in ) :: value(:)

    this% key = key
    select type( value )
      type is( integer )
        !this% object = objects( value )
        this% object = ivalue( value )
      class default
        !ERROR( "Value kind is not taking into account" )
        print*, "Value kind is not taking into account" 
    end select

  end function pair_rk1_constructor



  !...................................................................
  function getkey( self )result( key )
    class( pair ), intent( in ) :: self
    character(:), allocatable :: key
    key = self% key
  end function getkey

  function getval( self )result( val )
    class(c_object), allocatable :: val
    class( pair ), intent( in ) :: self
    val = self% object
  end function getval


  !...................................................................
  subroutine assign( lhs, rhs )
     type(pair), intent(out) :: lhs
     type(pair), intent(in)  :: rhs
     lhs% key = rhs% key
     lhs% object = rhs% object
  end subroutine assign


end module m_pair








