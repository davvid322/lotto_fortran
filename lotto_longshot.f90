! ----------------------------------------------------------------------------
! Lotto Longshot - a Lotto 6/49 simulator that shows the futility of lotteries
! Created by David Young May 2023 using GNU Fortran 11.3.0 on Ubuntu Linux 22.04
! 
program lotto_longshot
  use lotto_subs
  implicit none
  
  call random_seed ()
  call get_user_input ()
  call run_simulation ()
  call report_results ()
  
end program lotto_longshot
