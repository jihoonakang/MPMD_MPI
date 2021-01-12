# Multiple Program Multiple Data (MPMD) programming with MPI

This contrains fortran example codes for MPMD programming with MPI. Four example cases with MPI implementations are provided. 

## 1. Communicatior split with MPI_Comm_split function
<img src=https://user-images.githubusercontent.com/34472850/104287296-98147700-54f9-11eb-921e-2aa446ca7168.png>
## 2. Process spawn with MPI_Comm_spawn function
<img src=https://user-images.githubusercontent.com/34472850/104287298-98147700-54f9-11eb-81d0-7a88debce576.png>
## 3. Process spawn with MPI_Comm_spawn_multiple function and communicatior split with MPI_Comm_split function
<img src=https://user-images.githubusercontent.com/34472850/104287289-964ab380-54f9-11eb-97ec-00de9c1cbf65.png>
## 4. Process spawn with MPI_Comm_spawn function and inter-communicator creation between spawned processes
<img src=https://user-images.githubusercontent.com/34472850/104287294-977be080-54f9-11eb-8a98-a1a47b5ac367.png>

All codes have been compiled and tested with intel Fortran compiler 2021.1 Beta and Intel MPI 2021.1-beta10 on Ubuntu 18.04 LTS.
