
!==========================================================================================================
!> @brief       MPI MPMD master program - Spawn woker process
!> @detail      This program is a part of MPI MPMD tutorial code using MPI_Comm_spawn_multiple.
!> @author      Ji-Hoon Kang (jhkang@kisti.re.kr), Korea Institute of Science and Technology Information
!> @date        01 January 2021
!> @version     0.1
!> @par         Copyright
!>              Copyright (c) 2021 by Ji-Hoon Kang. All rights reserved.
!> @par         License     
!>              This project is release under the terms of the MIT License (see LICENSE in )
!==========================================================================================================

program master

    use mpi_f08
    use mpmd, only: mp_comm_size

    implicit none

    type(MPI_INFO)      :: info(2)
    type(MPI_COMM)      :: child_comm
    integer(kind=4)     :: child_np(2)
    character(len=32)   :: child_name(2)
    character(len=32)   :: array_of_argv(2,2)
    integer(kind=4)     :: fact_recv, sum_recv, root
    type(MPI_REQUEST)   :: req(2)

    call MPI_Init()
    call MPI_Info_create(info(1))
    call MPI_Info_set(info(1),"add-host","jihoon")
    call MPI_Info_create(info(2))
    call MPI_Info_set(info(2),"add-host","jihoon")

    child_name(1) = './factorial.ex'
    child_name(2) = './sum.ex'

    child_np(1) = mp_comm_size(1)
    child_np(2) = mp_comm_size(2)

    array_of_argv(1,1) = '10'
    array_of_argv(1,2) = ''
    array_of_argv(2,1) = '100'
    array_of_argv(2,2) = ''

    root = 0
    call MPI_Comm_spawn_multiple(2, child_name, array_of_argv, child_np, info, root, MPI_COMM_WORLD, child_comm, MPI_ERRCODES_IGNORE)

    call MPI_Irecv(fact_recv, 1, MPI_INTEGER, 0, 1, child_comm, req(1))
    call MPI_Irecv(sum_recv, 1, MPI_INTEGER, mp_comm_size(1), 1, child_comm, req(2))
    call MPI_Waitall(2, req, MPI_STATUSES_IGNORE)

    print '(a2,a15,a2,a10,a3,i20,a20)', ' [','./master.ex','] ',trim(array_of_argv(1,1)),'!= ',fact_recv,' (from factorial.ex)'
    print '(a2,a15,a8,a4,a3,i20,a14)', ' [','./master.ex','] Sigma ',trim(array_of_argv(2,1)),' = ',sum_recv,' (from sum.ex)'

    call MPI_Finalize()

end program master

