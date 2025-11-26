
module m_objects

  use iso_c_binding, only : c_ptr, c_null_ptr, &
                     c_loc, c_f_pointer, c_associated
  implicit none

  private
  public :: c_object, objects, ivalue

  ! Base Type for C object
  type c_object
    type(c_ptr) :: object = c_null_ptr
    character :: kind  ! 'i'=integer, 'r'=real, 'd'=double, 'c'=complex
    integer :: rank
    integer, allocatable :: sizes(:)
   contains
    FINAL :: destroy_c_object
  end type c_object

  type, extends(c_object) :: ivalue
    integer, pointer :: val(:) => null()
   contains
    FINAL :: destroy_ivalue
  end type ivalue


  interface objects
    !module procedure :: c_object
    module procedure :: ivalue
  end interface objects

  interface ivalue
    module procedure :: ival_rk0_constructor
    module procedure :: ival_rk1_constructor
  end interface ivalue

  interface assignment(=)
    module procedure :: copy_ival
    module procedure :: copy_cobj
  end interface assignment(=)

 CONTAINS

  !..............................................................
  type( ivalue ) function ival_rk0_constructor( val )result( this )
    integer, intent(in) :: val
    integer :: s

    print*, " > ival_rk0_constructor: ", val

    allocate( this% val(1) )
    this% val(1) = val
    this% object = c_loc(this% val)
    print*, " > ival_rk0_constructor: ", associated(this% val), c_associated(this% object)

    this% kind = 'i'
    this% sizes = shape(this% val)
    this% rank = 0
  end function ival_rk0_constructor

  !..............................................................
  type( ivalue ) function ival_rk1_constructor( val )result( this )
    integer, intent(in) :: val(:)
    integer :: s
    
    allocate( this% val, source=val )
    this% object = c_loc(this% val)

    this% kind = 'i'
    this% sizes = shape(this% val)
    this% rank = size(this% sizes)
  end function ival_rk1_constructor

  !..............................................................
  subroutine destroy_c_object( this )
    type(c_object) :: this
    if( c_associated(this% object) ) this% object = c_null_ptr 
  end subroutine destroy_c_object

  subroutine destroy_ivalue( this )
    type(ivalue) :: this
    if( associated(this% val) )deallocate(this% val)
  end subroutine destroy_ivalue

  !..............................................................
  subroutine copy_ival( lhs, rhs )
     type(ivalue), intent(out) :: lhs
     type(ivalue), intent(in)  :: rhs
     allocate( lhs% val, source=rhs% val )
     lhs% c_object = rhs% c_object
     lhs% object = c_loc(lhs% val)
  end subroutine copy_ival

  subroutine copy_cobj( lhs, rhs )
     type(c_object), intent(out) :: lhs
     type(c_object), intent(in)  :: rhs
     !lhs% object = rhs% object
     lhs% kind   = rhs% kind
     lhs% sizes  = rhs% sizes
     lhs% rank   = rhs% rank
  end subroutine copy_cobj

end module m_objects





