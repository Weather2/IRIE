#include "mpi.h"
#include "wtime.h"
#include <stdlib.h>
#include <stdio.h>



void  mpi_error( void )
{
    printf( "mpi_error called\n" );
    abort();
}




int   MPI_Irecv( void         *buf,
                 int          count,
                 MPI_Datatype datatype,
                 int          source,
                 int          tag,
                 MPI_Comm     comm,
                 MPI_Request  *request )
{
    mpi_error();
    return( MPI_ERR_OTHER );
}




int   MPI_Send( void         *buf,
                int          count,
                MPI_Datatype datatype,
                int          dest,
                int          tag,
                MPI_Comm     comm )
{
    mpi_error();
    return( MPI_ERR_OTHER );
}




int   MPI_Wait( MPI_Request *request,
                MPI_Status  *status )
{
    mpi_error();
    return( MPI_ERR_OTHER );
}




int   MPI_Init( int  *argc,
                char ***argv )
{
    return( MPI_SUCCESS );
}




int   MPI_Comm_rank( MPI_Comm comm, 
                     int      *rank )
{
    *rank = 0;
    return( MPI_SUCCESS );
}




int   MPI_Comm_size( MPI_Comm comm, 
                     int      *size )
{
    *size = 1;
    return( MPI_SUCCESS );
}




double MPI_Wtime( void )
{
    void wtime();

    double t;
    wtime( &t );
    return( t );
}




int  MPI_Barrier( MPI_Comm comm )
{
    return( MPI_SUCCESS );
}




int  MPI_Finalize( void )
{
    return( MPI_SUCCESS );
}




int  MPI_Allreduce( void         *sendbuf,
                    void         *recvbuf,
                    int          nitems,
                    MPI_Datatype type,
                    MPI_Op       op,
                    MPI_Comm     comm )
{
    int i;
    if( type == MPI_INT )
    {
        int *pd_sendbuf, *pd_recvbuf;
        pd_sendbuf = (int *) sendbuf;    
        pd_recvbuf = (int *) recvbuf;    
        for( i=0; i<nitems; i++ )
            *(pd_recvbuf+i) = *(pd_sendbuf+i);
    }
    if( type == MPI_DOUBLE )
    {
        double *pd_sendbuf, *pd_recvbuf;
        pd_sendbuf = (double *) sendbuf;    
        pd_recvbuf = (double *) recvbuf;    
        for( i=0; i<nitems; i++ )
            *(pd_recvbuf+i) = *(pd_sendbuf+i);
    }
    return( MPI_SUCCESS );
}
  



int  MPI_Reduce( void         *sendbuf,
                 void         *recvbuf,
                 int          nitems,
                 MPI_Datatype type,
                 MPI_Op       op,
                 int          root,
                 MPI_Comm     comm )
{
    int i;
    if( type == MPI_INT )
    {
        int *pi_sendbuf, *pi_recvbuf;
        pi_sendbuf = (int *) sendbuf;    
        pi_recvbuf = (int *) recvbuf;    
        for( i=0; i<nitems; i++ )
            *(pi_recvbuf+i) = *(pi_sendbuf+i);
    }
    if( type == MPI_DOUBLE )
    {
        double *pd_sendbuf, *pd_recvbuf;
        pd_sendbuf = (double *) sendbuf;    
        pd_recvbuf = (double *) recvbuf;    
        for( i=0; i<nitems; i++ )
            *(pd_recvbuf+i) = *(pd_sendbuf+i);
    }
    return( MPI_SUCCESS );
}
  



int  MPI_Alltoall( void         *sendbuf,
                   int          sendcount,
                   MPI_Datatype sendtype,
                   void         *recvbuf,
                   int          recvcount,
                   MPI_Datatype recvtype,
                   MPI_Comm     comm )
{
    int i;
    if( recvtype == MPI_INT )
    {
        int *pd_sendbuf, *pd_recvbuf;
        pd_sendbuf = (int *) sendbuf;    
        pd_recvbuf = (int *) recvbuf;    
        for( i=0; i<sendcount; i++ )
            *(pd_recvbuf+i) = *(pd_sendbuf+i);
    }
    return( MPI_SUCCESS );
}
  



int  MPI_Alltoallv( void         *sendbuf,
                    int          *sendcounts,
                    int          *senddispl,
                    MPI_Datatype sendtype,
                    void         *recvbuf,
                    int          *recvcounts,
                    int          *recvdispl,
                    MPI_Datatype recvtype,
                    MPI_Comm     comm )
{
    int i;
    if( recvtype == MPI_INT )
    {
        int *pd_sendbuf, *pd_recvbuf;
        pd_sendbuf = (int *) sendbuf;    
        pd_recvbuf = (int *) recvbuf;    
        for( i=0; i<sendcounts[0]; i++ )
            *(pd_recvbuf+i+recvdispl[0]) = *(pd_sendbuf+i+senddispl[0]);
    }
    return( MPI_SUCCESS );
}
  



