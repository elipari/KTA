/*****************************************************************
 *** segyheader.h MUST be #include(d) in the FORTRAN main prog ***
 *****************************************************************

 ----------------------------------------------------------
 --- REEL IDENTIFICATION HEADER (reelheader() variable) ---
 ----------------------------------------------------------       */
#define SEGY_BIN_JOBN 1;
/*                    +-> Job identification number*/
#define SEGY_BIN_LINE 2;
/*                     +-> Line Number*/
#define SEGY_BIN_REEL 3;
/*                     +-> Reel Number*/
#define SEGY_BIN_NTRA 4;
/*                     +-> Number of data traces per record*/
#define SEGY_BIN_NAUX 5;
/*                     +-> Number of auxiliary traces per record*/
#define SEGY_BIN_TSAM 6;
/*                     +-> Sample interval in microsec*/
#define SEGY_BIN_TSOR 7;
/*                     +-> Sample interval (original field rec)*/
#define SEGY_BIN_NSAM 8;
/*                     +-> Number of samples per data trace*/
#define SEGY_BIN_FORM 10;
/*                     +-> Data sample format code (1 = float)*/
#define SEGY_BIN_CDPF 11;
/*                     +-> CDP fold*/
#define SEGY_BIN_SORT 12;
/*                     +-> Trace sorting code */
#define SEGY_BIN_VSUM 13;
/*                     +-> Vertical sum code*/
#define SEGY_BIN_GAIN 23;
/*                     +-> Binary gain recovered*/
#define SEGY_BIN_AMPL 24;
/*                     +-> Amplitude recovery method*/
#define SEGY_BIN_MSYS 25;
/*                     +-> Measurement system (1 = meters)*/
#define SEGY_BIN_POLA 26;
/*                     +-> Polarity

! ------------------------------------------------------------
! --- TRACE IDENTIFICATION HEADER (traceheader() variable) ---
! ------------------------------------------------------------*/
#define SEGY_TRA_TRSN 1;
/*                     +-> Trace sequence number within line*/
#define SEGY_TRA_TRSR 2;
/*                     +-> Trace sequence number within reel*/
#define SEGY_TRA_FIRN 3;
/*                     +-> Original field record number*/
#define SEGY_TRA_TRAN 4;
/*                     +-> Trace number within the original field rec*/
#define SEGY_TRA_CDPN 6;
/*                     +-> CDP ensamble number*/
#define SEGY_TRA_CDPT 7;
/*                     +-> Trace number within the CDP ensemble*/
#define SEGY_TRA_TRIC 8;
/*                     +-> Trace identification code (1 = seismic)*/
#define SEGY_TRA_VSTK 9;
/*                    +-> No. of vertically summed traces yielding this trace*/
#define SEGY_TRA_HSTK 10;
/*                     +-> No. of horizont. stacked traces yielding this trace*/
#define SEGY_TRA_DUSE 11;
/*                     +-> Data use*/
#define SEGY_TRA_DIST 12;
/*                     +-> Dist. from source point to receiver group*/
#define SEGY_TRA_RXGE 13;
/*                     +-> Receiver group elevation*/
#define SEGY_TRA_SDEP 15;
/*                     +-> Source depth below surface*/
#define SEGY_TRA_ELSC 20;
/*                     +-> Scaler to be applied to all elevations & depths...*/
#define SEGY_TRA_COSC 21;
/*                     +-> Scaler to be applied to all coords specified...*/
#define SEGY_TRA_XSOU 22;
/*                     +-> Source coordinate X*/
#define SEGY_TRA_YSOU 23;
/*                     +-> Source coordinate Y*/
#define SEGY_TRA_XGRP 24;
/*                     +-> Group coordinate X*/
#define SEGY_TRA_YGRP 25;
/*                     +-> Group coordinate Y*/
#define SEGY_TRA_CUNI 26;
/*                     +-> Coordinate units*/
#define SEGY_TRA_LAGA 34;
/*                     +-> Lag time A*/
#define SEGY_TRA_LAGB 35;
/*                     +-> Lag time B*/
#define SEGY_TRA_RDEL 36;
/*                     +-> Delay recording time*/
#define SEGY_TRA_NSAM 39;
/*                     +-> Number of samples in this trace*/
#define SEGY_TRA_TSAM 40;
/*                     +-> Sample interval in microsec*/
#define SEGY_TRA_YEAR 60;
/*                     +-> Year data recorded*/
#define SEGY_TRA_DAYS 61;
/*                     +-> Day of year*/
#define SEGY_TRA_HOUR 62;
/*                     +-> Hour of day*/
#define SEGY_TRA_MINS 63;
/*                     +-> Minute of hour*/
#define SEGY_TRA_SECS 64;
/*                     +-> Second of minute*/
#define SEGY_TRA_TCOD 65;
/*                     +-> Time basis code*/
#define SEGY_TRA_TWFA 66;
/*                     +-> Trace weighting factor*/
#define SEGY_TRA_INLI 88;
/*                     +-> Inline number                 */
#define SEGY_TRA_XLIN 89;
/*                    +-> Crossline number               */
/**/

