program example_sort_index_real
  use stdlib_sorting, only: sort_index
  implicit none
  integer, allocatable :: array(:)
  real, allocatable :: index(:)

  array = [5, 4, 3, 1, 10, 4, 9]
  index = real(array)

  call sort_index(array, index)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, index   !print [1., 3., 4., 4., 5., 9., 10.]

end program example_sort_index_real
