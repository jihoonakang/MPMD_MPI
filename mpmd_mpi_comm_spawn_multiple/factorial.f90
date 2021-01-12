
!==========================================================================================================
!> @brief       MPI MPMD worker - parallel factorial calculation
!> @detail      This program is a part of MPI MPMD tutorial code.
!> @author      Ji-Hoon Kang (jhkang@kisti.re.kr), Korea Institute of Science and Technology Information
!> @date        01 January 2021
!> @version     0.1
!> @par         Copyright
!>              Copyright (c) 2021 by Ji-Hoon Kang. All rights reserved.
!> @par         License     
!>              This project is release under the terms of the MIT License (see LICENSE in )
!==========================================================================================================

program factorial

    use mpi_f08
    use mpmd

    implicit none

    integer(kind=4) :: nprocs, myrank
    integer(kind=4) :: i, ista, iend
    integer(kind=4) :: n, n_sum
    integer(kind=4) :: fact_local, fact_global, sum_global
    character(len=255)  :: arg, command

    type(MPI_COMM)  :: local_comm, master_comm
    integer(kind=4) :: local_size, local_rank

    call MPI_Init()

    call MPI_Comm_size(MPI_COMM_WORLD, nprocs)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank)

    call mpmd_comm_init(MPI_COMM_WORLD, nprocs, myrank, local_comm, local_size, local_rank)
    call MPI_Comm_get_parent(master_comm)

    if (command_argument_count() /= 1) then
        call get_command_argument(0, command)
        if(local_rank.eq.0) print *, '[Factorial] Usage:', trim(command), ' n (integer)'
        stop
    else
        call get_command_argument(0, command)
        call get_command_argument(1, arg)
        read(arg, *) n
    endif

    call para_range(1, n, local_size, local_rank, ista, iend)

    fact_local = 1
    do i = ista, iend
        fact_local = fact_local * i
    enddo

    call MPI_Reduce(fact_local, fact_global, 1, MPI_INTEGER, MPI_PROD, 0, local_comm)

    if(local_rank.eq.0) then
        print '(a2,a15,a2,i10,a3,i20)', ' [',trim(command),'] ',n,'!= ',fact_global
        call MPI_Send(fact_global, 1, MPI_INTEGER, 0, 1, master_comm)
    endif

    call MPI_Finalize()

end program