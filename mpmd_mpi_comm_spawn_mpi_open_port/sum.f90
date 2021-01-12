
!==========================================================================================================
!> @brief       MPI MPMD worker - parallel factorial calculation
!> @detail      This program is a part of MPI MPMD tutorial code using MPI_Comm_spawn with port connection.
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

    implicit none

    type(MPI_COMM)      :: master_comm
    type(MPI_COMM)      :: worker_comm
    type(MPI_COMM)      :: local_comm
    integer(kind=4)     :: local_rank, local_size

    integer(kind=4)     :: i, ista, iend
    integer(kind=4)     :: n
    integer(kind=4)     :: sum_local, sum_global, fact_global, n_fact
    character(len=255)  :: arg, command
    character(len=MPI_MAX_PORT_NAME)  :: portname

    call MPI_Init()

    call MPI_Comm_dup(MPI_COMM_WORLD, local_comm)
    call MPI_Comm_size(local_comm, local_size)
    call MPI_Comm_rank(local_comm, local_rank)
    call MPI_Comm_get_parent(master_comm)

    if(local_rank.eq.0) then
        call MPI_Recv(portname, MPI_MAX_PORT_NAME, MPI_CHARACTER, 0, 2, master_comm, MPI_STATUS_IGNORE)

        ! caution : use MPI_COMM_SELF, not MPI_COMM_WORLD for port connection
        ! create MPI_COMM_WORKER0 inter-communicator with pi.ex
        call MPI_Comm_connect(portname, MPI_INFO_NULL, 0, MPI_COMM_SELF, worker_comm)
    endif

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
    
    if(local_rank.eq.0) then
        print '(a2,a15,a8,i4,a3,i20)', ' [',trim(command),'] Sigma ',n,' = ',sum_global
        call MPI_Send(sum_global, 1, MPI_INTEGER, 0, 1, master_comm)
        call MPI_Recv(n_fact, 1, MPI_INTEGER, 0, 2, worker_comm, MPI_STATUS_IGNORE)
        call MPI_Recv(fact_global, 1, MPI_INTEGER, 0, 1, worker_comm, MPI_STATUS_IGNORE)
        call MPI_Send(n, 1, MPI_INTEGER, 0, 2, worker_comm)
        call MPI_Send(sum_global, 1, MPI_INTEGER, 0, 1, worker_comm)
        print '(a2,a15,a2,i10,a3,i20,a20)', ' [',trim(command),'] ',n_fact,'!= ',fact_global,' (from factorial.ex)'
    endif

    if(local_rank.eq.0) then
        call MPI_Comm_disconnect(worker_comm)
    endif

    call MPI_Finalize()

end program