
!==========================================================================================================
!> @brief       MPI MPMD module - Create local communicators using MPI_Comm_split
!> @detail      This program is a part of MPI MPMD tutorial code.
!> @author      Ji-Hoon Kang (jhkang@kisti.re.kr), Korea Institute of Science and Technology Information
!> @date        01 January 2021
!> @version     0.1
!> @par         Copyright
!>              Copyright (c) 2021 by Ji-Hoon Kang. All rights reserved.
!> @par         License     
!>              This project is release under the terms of the MIT License (see LICENSE in )
!==========================================================================================================

module mpmd

    use mpi_f08
    implicit none

    integer(kind=4), parameter  :: mp_comm_size(2) = (/4, 8/)
    integer(kind=4)             :: color, global_rank, global_size

    contains

    subroutine mpmd_comm_init(oldcomm, oldsize, oldrank, newcomm, newsize, newrank)

        use mpi_f08
        implicit none

        type(MPI_Comm), intent(in)      :: oldcomm
        type(MPI_Comm), intent(inout)   :: newcomm
        integer(kind=4), intent(in)     :: oldsize, oldrank
        integer(kind=4), intent(inout)  :: newsize, newrank

        global_rank = oldrank
        global_size = oldsize

        if(oldrank.LT.mp_comm_size(1)) then
            color = 1
        else
            color = 2
        endif

        call MPI_Comm_split(oldcomm, color, oldrank, newcomm)
        call MPI_Comm_size(newcomm, newsize)
        call MPI_Comm_rank(newcomm, newrank)
        if(newsize.ne.mp_comm_size(color)) then
            print *, '[Error] Number of mpi processes does not matches to the assigned size.'
            stop
        endif
        print *, '[MPMD] myrank = ',oldrank, ' newrank = ',newrank

    end subroutine mpmd_comm_init

    subroutine mpmd_send_a_recv_b(a, b)

        use mpi_f08
        implicit none

        integer(kind=4), intent(in)     :: a
        integer(kind=4), intent(inout)  :: b

        type(MPI_STATUS)    :: status

        if(global_rank.eq.0) then
            call MPI_Send(a, 1, MPI_INTEGER, mp_comm_size(1), 1, MPI_COMM_WORLD)
            call MPI_Recv(b, 1, MPI_INTEGER, mp_comm_size(1), 1, MPI_COMM_WORLD, status)
        endif
    
        if(global_rank.eq.mp_comm_size(1)) then
            call MPI_Recv(b, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, status)
            call MPI_Send(a, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD)
        endif
    
    end subroutine

end module
