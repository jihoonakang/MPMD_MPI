
!==========================================================================================================
!> @brief       MPI MPMD worker - parallel summation
!> @detail      This program is a part of MPI MPMD tutorial code.
!> @author      Ji-Hoon Kang (jhkang@kisti.re.kr), Korea Institute of Science and Technology Information
!> @date        01 January 2021
!> @version     0.1
!> @par         Copyright
!>              Copyright (c) 2021 by Ji-Hoon Kang. All rights reserved.
!> @par         License     
!>              This project is release under the terms of the MIT License (see LICENSE in )
!==========================================================================================================

program sum

    use mpi_f08
    use mpmd

    implicit none

    integer(kind=4) :: nprocs, myrank
    integer(kind=4) :: i, ista, iend
    integer(kind=4) :: n, n_fact
    integer(kind=4) :: sum_local, sum_global, fact_global
    character(len=255) :: arg, command

    type(MPI_COMM)  :: local_comm
    integer(kind=4) :: local_size, local_rank

    call MPI_Init()

    call MPI_Comm_size(MPI_COMM_WORLD, nprocs)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank)

    call mpmd_comm_init(MPI_COMM_WORLD, nprocs, myrank, local_comm, local_size, local_rank)

    if (command_argument_count() /= 1) then
        call get_command_argument(0, command)
        if(local_rank.eq.0) print *, '[Sum] Usage:', trim(command), ' n (integer)'
        stop
    else
        call get_command_argument(0, command)
        call get_command_argument(1, arg)
        read(arg, *) n
    endif

    call para_range(1, n, local_size, local_rank, ista, iend)

    sum_local = 0
    do i = ista, iend
        sum_local = sum_local + i
    enddo

    call MPI_Reduce(sum_local, sum_global, 1, MPI_INTEGER, MPI_SUM, 0, local_comm)
    
    call mpmd_send_a_recv_b(sum_global, fact_global)
    call mpmd_send_a_recv_b(n, n_fact)

    if(local_rank.eq.0) then
        print '(a2,a15,a8,i4,a3,i20)', ' [',trim(command),'] Sigma ',n,' = ',sum_global
        print '(a2,a15,a2,i10,a3,i20,a20)', ' [',trim(command),'] ',n_fact,'!= ',fact_global,' (from factorial.ex)'
    endif

    call MPI_Finalize()

end program