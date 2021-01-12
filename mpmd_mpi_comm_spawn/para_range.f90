
!==========================================================================================================
!> @brief       Assigns the computing range to myrank among total nprocs
!> @detail      This program is a part of MPI MPMD tutorial code.
!> @author      Ji-Hoon Kang (jhkang@kisti.re.kr), Korea Institute of Science and Technology Information
!> @date        01 January 2021
!> @version     0.1
!> @par         Copyright
!>              Copyright (c) 2021 by Ji-Hoon Kang. All rights reserved.
!> @par         License     
!>              This project is release under the terms of the MIT License (see LICENSE in )
!==========================================================================================================

!> @brief       Compute the indices of the assigned range for each MPI process .
!> @param       n1      First index of total range
!> @param       n2      Last index of total range
!> @param       nprocs  Number of MPI process
!> @param       myrank  Rank ID of my MPI process
!> @param       ista    First index of assigned range for myrank
!> @param       iend    Last index of assigned range for myrank

subroutine para_range(n1, n2, nprocs, myrank, ista, iend)

    implicit none

    integer, intent(in)     :: n1, n2, nprocs, myrank
    integer, intent(out)    :: ista, iend
    integer :: iwork1, iwork2

    iwork1 = int((n2 - n1 + 1) / nprocs)
    iwork2 = mod(n2 - n1 + 1, nprocs)
    ista = myrank * iwork1 + n1 + min(myrank, iwork2)
    iend = ista + iwork1 - 1
    if (iwork2 > myrank) iend = iend + 1

end subroutine para_range