

program test_dict

  use m_dict, only : dict
  implicit none

  type( dict ) :: dico

  dico = dict(name="test")

  call dico% add( "hello", 10 )
  !call dico% set( "double", 1.9 )
  !call dico% set( "double", "banane" )

  call dico% print()

end program test_dict


