subroutine SUB_SAVETXT_(filename, d)
  ! Saves a 2D array into a textfile.
  !
  ! Arguments
  ! ---------
  !
  character(len=*), intent(in) :: filename  ! File to save the array to
  TYPE_, intent(in) :: d(:,:)           ! The 2D array to save
  !
  ! Example
  ! -------
  !
  ! TYPE_ :: data(3, 2)
  ! call savetxt("log.txt", data)
  
  integer :: s, i

  open(newunit=s, file=filename, status="replace")
  do i = 1, size(d, 1)
    write(s, *) d(i, :)
  end do

  close(s)

end subroutine

