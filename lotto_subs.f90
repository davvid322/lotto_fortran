module lotto_subs
    use, intrinsic :: iso_fortran_env
    implicit none

    ! global constants
    integer, parameter :: MIN_BALL = 1
    integer, parameter :: MAX_BALL = 49
    integer, parameter :: MAX_PICKS = 6
    integer, parameter :: RESULT_SCENARIOS = 7
    integer, parameter :: COST_PER_TICKET = 3
    integer, parameter, dimension(RESULT_SCENARIOS) :: PAYOFF_RATES = &
        [0, 0, 3, 10, 80, 2500, 9000000]
    
    ! global variables
    logical :: quick_picks = .true.
    character(8) :: start_date, end_date
    character(10) :: start_time, end_time
    real :: runtime_seconds
    integer, dimension(MAX_PICKS) :: my_picks
    integer(8) :: num_games_to_run
    integer(8), dimension(RESULT_SCENARIOS) :: count_results = 0
    integer(8), dimension(MAX_BALL) :: fair_balls_count = 0
    
    contains
    subroutine get_user_input ()
        character(1) :: quick_pick_choice
        character(20) :: comma_str
        write (*,*) 'Welcome to Lotto Longshot - a lesson in futility'
        write (*,*) '------------------------------------------------'
        write (*,*) '-- For up to 100 simulated games, details of each game will be shown.'
        write (*,*) '-- For more than 100 games, only summary statistics are listed.'
        write (*,*) '-- The number of times each ball came up is shown at the end (fairness check).'
        write (*,*)
        write (*,*) 'Press y + Enter for a random quick pick, or any other letter &
            to choose your own set of numbers:'
        read (*,*) quick_pick_choice
        if (quick_pick_choice == 'y') then
            quick_picks = .true.
            call do_quick_pick(my_picks)
        else 
            quick_picks = .false.
            call get_user_picks(my_picks)
        end if
        sort_picks : block  ! bubblesort
        integer :: i, j, temp
            do i = 1, MAX_PICKS-1
                do j = 1, MAX_PICKS-i
                    if (my_picks(j) > my_picks(j+1)) then
                        ! Swap elements
                        temp = my_picks(j)
                        my_picks(j) = my_picks(j+1)
                        my_picks(j+1) = temp
                    end if
                end do
            end do
        end block sort_picks
        write (*,*) 'How many games to you want to run?'
        read (*,*) num_games_to_run
    end subroutine get_user_input

    subroutine do_quick_pick (picks)
        integer, dimension(MAX_PICKS), intent(out) :: picks
        
        logical, dimension(MAX_BALL) :: quick_balls_array = .false.
        integer :: myballs_picked = 0, n = 0
        call draw_balls (quick_balls_array)
        do while (myballs_picked < MAX_PICKS)
            n = n + 1
            if (quick_balls_array(n)) then
                myballs_picked = myballs_picked + 1
                picks(myballs_picked) = n
            end if
        end do
    end subroutine do_quick_pick

    subroutine draw_balls (balls_array)
        logical, dimension(MAX_BALL), intent(out) :: balls_array
        integer :: nballs_picked, test_ball
        real :: r
        balls_array = .false.
        nballs_picked = 0
        do while (nballs_picked < MAX_PICKS)
            call random_number (r)
            test_ball = (r * MAX_BALL) + 1
            if (.not. balls_array(test_ball)) then
                balls_array(test_ball) = .true.
                nballs_picked = nballs_picked + 1
                fair_balls_count(test_ball) = fair_balls_count(test_ball) + 1
            end if
        end do
    end subroutine draw_balls
    
    subroutine get_user_picks (picks)
        integer, dimension(MAX_PICKS), intent(out) :: picks
        logical :: valid = .false.
        integer :: m, n
        edit_checks: do while (.not. valid)
            write (*,fmt='("Enter ", i2, " numbers from ", i2, " to ", i2)') &
                MAX_PICKS, MIN_BALL, MAX_BALL
            read (*,*) picks
            do n = 1, MAX_PICKS
                if ((picks(n) < MIN_BALL) .or. (picks(n) > MAX_BALL)) then
                    write (*,fmt='("You chose ", i2, " but numbers must be from ", &
                    i2, " to ", i2)') picks(n), MIN_BALL, MAX_BALL
                    cycle edit_checks
                end if
                do m = 1, MAX_PICKS
                    if (( n /= m) .and. (picks(n) == picks(m))) then
                        write (*,fmt='("You chose duplicate number ", i2)') picks(n)
                        cycle edit_checks
                    end if
                end do
            end do
            valid = .true.  ! Passed all edit checks
        end do edit_checks
    end subroutine get_user_picks

    subroutine run_simulation ()
        real, dimension(2) :: tarray
        real :: tresult
        integer(8) :: n, progress_counter
        integer :: n_right
        call dtime(tarray, tresult)  ! Initialize runtime counters
        call date_and_time (DATE = start_date)
        call date_and_time (TIME = start_time)
        write (*, fmt='("Running simulation for : ", a, " games at ", a, " - ", a)') &
            add_commas(num_games_to_run), start_date, start_time
        write (*, fmt='("Numbers chosen : ", 6(i2,1x))') my_picks
        progress_counter = 0
        do n = 1, num_games_to_run
            progress_counter = progress_counter + 1
            if (progress_counter >= 10000000) then ! Show progress while running
                write (*,*) "...Running game : ", add_commas(n)
                progress_counter = 0
            end if
            call run_a_game (n_right)
            ! Note that result counters are indexed 1..7; num_right can be 0..6
            count_results(n_right + 1) = count_results(n_right + 1) + 1
        end do
        
        call date_and_time (DATE = end_date)
        call date_and_time (TIME = end_time)
        write (*, fmt='("Simulation Ended   : ", a, " - ", a)') end_date, end_time
        call dtime(tarray, tresult)  ! Retrieve and save the runtime in seconds
        runtime_seconds = tresult
        
    end subroutine run_simulation

    subroutine run_a_game (num_right)
        integer, intent(out) :: num_right
        logical, dimension(MAX_BALL) :: run_balls_array = .false.
        integer, dimension(MAX_PICKS) :: balls_picked_list
        integer :: n, m
        num_right = 0
        call draw_balls (run_balls_array)
        do n = 1, MAX_PICKS
            if (run_balls_array(my_picks(n))) then
                num_right = num_right + 1
            end if
        end do
        if (num_games_to_run <= 100) then  ! Show details for a small number of runs
            m = 0
            do n = 1, MAX_BALL
                if (run_balls_array(n)) then
                    m = m + 1
                    balls_picked_list(m) = n
                end if
            end do
            write (*, fmt='("Game draw : ", 6(i2,1x), "  You got ", &
                i2, " right")') balls_picked_list, num_right
        end if
    end subroutine run_a_game

    subroutine report_results ()
        real(8) :: profit_pct, realrunballs
        integer(8) :: total_cost, total_profit, this_payoff, total_payoff, runs_per_second
        integer :: n
        write (*,*) "FYI, here is a list of how often each ball was chosen"
        realrunballs = num_games_to_run * MAX_PICKS  ! Total number of drawings
        do n = 1, MAX_BALL
          write (*, fmt='("Ball ", i2, " was drawn ", a, " times, = ", f7.2, "%")') &
              n, add_commas(fair_balls_count(n)), ((fair_balls_count(n) / realrunballs) * 100.0)
        end do
        runs_per_second = num_games_to_run / runtime_seconds
        write (*, fmt='(a,f12.5)') "Runtime seconds : ", runtime_seconds
        write (*, fmt='(a,a)') "Runs per second : ", add_commas(runs_per_second)
        total_cost = 0
        total_payoff = 0
        do n = 1, RESULT_SCENARIOS
            this_payoff = count_results(n) * PAYOFF_RATES(n)
            total_payoff = total_payoff + this_payoff
            write (*, fmt='("You picked ", i2, " correct ", a, " times", &
                "  --> Payoff = $", a)') (n - 1), add_commas(count_results(n)), &
                add_commas(this_payoff)
        end do
        total_cost = num_games_to_run * COST_PER_TICKET
        total_profit = total_payoff - total_cost
        if (total_cost /= 0) then
            profit_pct = (((total_profit * 1.0) / (total_cost * 1.0)) * 100.0)
        else 
            profit_pct = 0.0
        end if
        write (*, fmt='("Total cost of tickets : $", a)') add_commas(total_cost)
        write (*, fmt='("Total money won       : $", a)') add_commas(total_payoff)
        write (*, fmt='("Total profit / loss   : $", a)') add_commas(total_profit)
        write (*, fmt='("Profit / loss percent :", f15.2, "%")') profit_pct
        if (profit_pct < 0) then
            write (*,*) "*** Loser!  I hoped you learned something from this! ***"
        else 
            write (*,*) "*** Winner!  Pure fluke though.  Don't make this a habit ***"
        end if
        write (*,*)
        write (*,*) "******************** End Simulation ********************"
        write (*,*)
    end subroutine report_results

    function add_commas (in_value) result (result_string)
        integer(8), intent(in) :: in_value
        character(:), allocatable :: result_string
        character(20) :: out_string
        character(20) :: in_string
        integer(8) :: abs_value
        integer, parameter :: STR_LEN = 20
        integer :: in_index, out_index, triplet_counter
        logical :: done = .false., add_minus = .false.
        if (in_value < 0) then ! add leading '-' after conversion
            add_minus = .true.
        end if
        write (in_string, fmt='(i20)') abs(in_value)
        out_string = in_string
        in_index = STR_LEN
        out_index = STR_LEN
        triplet_counter = 1
        do while (in_string(in_index:in_index) > ' ')
            if (triplet_counter > 3) then
                out_string(out_index:out_index) = ','
                out_index = out_index - 1
                triplet_counter = 1
            end if
            out_string(out_index:out_index) = in_string(in_index:in_index)
            in_index = in_index - 1
            out_index = out_index - 1
            triplet_counter = triplet_counter + 1
            if ((in_index < 1) .or. (out_index < 1)) then
               exit
            end if
        end do
        if (add_minus) then
            out_string(out_index:out_index) = '-'
        end if                
        result_string = trim(adjustl(out_string))
    end function add_commas

end module lotto_subs
