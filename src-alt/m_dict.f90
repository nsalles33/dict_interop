
module m_dict

  use m_pair
  implicit none

  integer, parameter :: addmem = 4

  type dict
    character(:), allocatable :: name
    type(pair), allocatable :: pairs(:)
    integer :: npairs = 0
    integer :: nmax = 0
  contains
    generic :: add => add_pair, add_keyval_0d, add_keyval_1d
    procedure :: add_pair, add_keyval_0d, add_keyval_1d
    !procedure :: get
    procedure :: find_key
    procedure :: print
  end type dict

  interface dict
    module procedure :: dict_constructor
  end interface


 CONTAINS

  !.................................................
  type(dict) function dict_constructor( name )result( this )
    character(*), intent(in), optional :: name
    character(*), parameter :: default = "not defined"

    if( present(name) )then
      this% name = trim(name)
    else
      this% name = default
    endif

    allocate( this% pairs(addmem) )
    this% nmax = addmem 

  end function dict_constructor


  !.................................................
  subroutine add_pair( self, p )
    class( dict ), intent( inout ) :: self
    type( pair ), intent( in ) :: p

    integer :: idx
    type( pair ), allocatable :: tmp(:)

    idx = self% find_key( p% getkey() ) 
    
    ! ...Already present, overwrite previous object
    if( idx > 0 )then
      self% pairs(idx) = p
      return
    endif

    ! ...New key but still some place
    if( self% npairs < self% nmax )then
      self% npairs = self% npairs + 1 
      self% pairs(self% npairs) = p

    ! ...New and need more place
    else
      ! Allocate
      tmp = self% pairs
      idx = self% nmax + 4 
      allocate( self% pairs(idx) )
      self% pairs(:self% nmax) = tmp(:)

      self% npairs = self% npairs + 1
      self% pairs(self% npairs) = p
      self% nmax = idx
    endif
     
  end subroutine add_pair

  !.................................................
  subroutine add_keyval_0D( self, key, val )
    class( dict ), intent( inout ) :: self
    character(*),  intent( in )    :: key
    class(*),      intent(in)      :: val

    integer :: idx
    type( pair ), allocatable :: tmp(:)

    idx = self% find_key( key )
    if( idx > 0 )then
      self% pairs(idx) = pair( key, val )
      return
    endif

    if(self% nmax == 0 )then
      ! New space
       allocate( self% pairs(addmem) )
       self% nmax = addmem

    else if( self% npairs >= self% nmax )then
      ! New space needed
      tmp = self% pairs
      idx = self% nmax + addmem
      allocate( self% pairs(idx) )
      self% pairs(:self%nmax) = tmp
      self% nmax = idx      

    endif

    print*, " > add_keyval: pairs: ", size(self% pairs)

    self% npairs = self% npairs + 1
    !self% pairs(self% npairs) = pair( key, val )
    self% pairs(self% npairs)% key = key
    !self% pairs(self% npairs)% object = objects(val)
    select type(val)
      type is(integer)
        self% pairs(self% npairs)% object = ivalue( val )
    end select

    print*, " > add_keyval: new pairs: ", self% pairs(self% npairs)% key

  end subroutine add_keyval_0D

  !.................................................
  subroutine add_keyval_1D( self, key, val )
    class( dict ), intent( inout ) :: self
    character(*),  intent( in )    :: key
    class(*),      intent(in)      :: val(:)

    integer :: idx
    type( pair ), allocatable :: tmp(:)

    idx = self% find_key( key )
    if( idx > 0 )then
      self% pairs(idx) = pair( key, val )
      return
    endif

    if( self% npairs < self% nmax )then
      self% npairs = self% npairs + 1
      self% pairs(self% npairs) = pair( key, val )

    else
      ! Allocate
      tmp = self% pairs
      idx = self% nmax + 4 
      allocate( self% pairs(idx) )
      self% pairs(:self%nmax) = tmp
      
      self% npairs = self% npairs + 1
      self% pairs(self% npairs) = pair( key, val )
      self% nmax = idx
    endif
      
  end subroutine add_keyval_1D


  !.................................................
  integer function find_key( self, key )result( idx )
    class(dict),  intent(in) :: self
    character(*), intent(in) :: key
    
    integer :: i

    idx = -1
    if (.not. allocated(self%pairs)) return

    do i = 1, self% npairs 
       if( self% pairs(i)% key == key )then
         idx = i
         return
       endif
    enddo

  end function find_key

  !.................................................
  subroutine print( self )
    class(dict), intent(in) :: self

    integer :: i

    write(*,*) " > Dictionary: ", self% name

    if( .not.allocated(self% pairs) )then
      write(*,*) " >> Dictionary Empty!"
      return
    endif

    do i = 1, self% npairs
       write(*,*) self% pairs(i)% key
    enddo

  end subroutine print

end module m_dict


