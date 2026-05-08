namespace Identi;

// ============================================================
// AppState – replaces all COMMON blocks from the original Fortran.
//
// Original COMMON blocks:
//   /KONFIG/  – hardware configuration and signal parameters
//   /STATUS/  – runtime acquisition counters
//   /ROHDAT/  – raw 16-bit measurement data ring buffer
//   /GRENZW/  – hardware A/D and D/A limits
//   /PTEXTE/  – channel text labels
//   /MODELL/  – identified model data
// ============================================================

internal sealed class AppState
{
    // ---- array dimension constants (original PARAMETER statements) ----
    public const int NDAMA  = 2;    // max D/A channels
    public const int NADMA  = 8;    // max A/D channels for model use
    public const int NADDAX = 10;   // total channels (AD + DA)
    public const int LENMA  = 1000; // ring-buffer length
    public const int NPWMA  = 400;  // max model parameter vector size

    // ---- COMMON/KONFIG/ ----
    // All arrays use 1-based indexing (element [0] unused) to match Fortran.
    public int   ITIME  = 10;   // clock interrupt value: 1, 10, or 600 (×100 ms)
    public int   ICNEND = 1;    // multiplier → sample time = ITIME*ICNEND*100 ms
    public int   NADU   = 1;    // number of A/D converter channels
    public int   NDAU   = 1;    // number of D/A converter channels
    public int   LEN    = 1000; // active length of the data field
    public int   ITYPMW = 0;    // mean-value computation mode (0-3)
    public int[]   IMW    = new int  [NADDAX + 1]; // mean values, indices 1..NADDAX
    public int[]   IVMA   = new int  [NDAMA  + 1]; // D/A amplitudes, indices 1..NDAMA
    public int[]   NRDM   = new int  [NDAMA  + 1]; // shift-register lengths
    public int[]   NSEQ   = new int  [NDAMA  + 1]; // sequence lengths
    public int[]   NRSB   = new int  [NDAMA  + 1]; // feedback register indices
    public int[]   IPL    = new int  [NADDAX + 1]; // plot channel indices
    public int    NPL    = 1;    // number of channels to plot
    public int    JOBPL  = 0;    // plot filter: 0=none,1=mean,2=diff,3=highpass
    public int[]   ISCALE = new int  [NADMA  + 1]; // channel scale factors
    public float  FPAR   = 0.99f;                   // high-pass filter parameter
    public float[] PMIN   = new float[NADDAX + 1]; // physical channel minima
    public float[] PMAX   = new float[NADDAX + 1]; // physical channel maxima

    // ---- COMMON/STATUS/ ----
    public int  IZ      = 0;  // total sample counter
    public int  ICNT    = 0;  // interrupt counter (reset every ICNEND interrupts)
    public int  NPOINT  = 0;  // circular-buffer write pointer (current position)
    public int  IONLFL  = 0;  // online acquisition flag (1 = running)
    public int  IZMOLD  = 0;  // IZ at last mean-value reset
    public int  NADRES  = 0;  // flat index (row-offset) into IDAT for current sample
    public int  ILAUF   = 0;  // loop index used inside the ISR context

    // ---- COMMON/ROHDAT/ ----
    // [channel, time], 1-based in both dimensions.
    public short[,] IDAT = new short[NADDAX + 1, LENMA + 1];

    // ---- COMMON/GRENZW/ ----
    public int NADMAX = 8;    // max A/D channels
    public int NDAMAX = 2;    // max D/A channels
    public int IADMIN = 0;    // A/D minimum raw value
    public int IADMAX = 4096; // A/D maximum raw value
    public int IDAMIN = 0;    // D/A minimum raw value
    public int IDAMAX = 2048; // D/A maximum raw value
    public int LENMAX = 1000; // maximum buffer length
    public int NPMAX  = 8;    // maximum plot channels

    // ---- COMMON/PTEXTE/ ----
    public string[] TEXT = new string[NADDAX + 1]; // channel titles, 1-based
    public string[] PBEZ = new string[NADDAX + 1]; // physical unit labels, 1-based

    // ---- COMMON/MODELL/ ----
    public int    JOBID = 0;  // last identification method used (1-4)
    public int    NANF  = 1;  // estimation start offset
    public int    IANZ  = 100;// number of estimation samples
    public int    N     = 0;  // system order (Σ NYI)
    public int    NM    = 1;  // number of model outputs
    public int    NU    = 1;  // number of model inputs
    public int    IGLW  = 0;  // 0 = no equilibrium offset, 1 = estimate offset
    public int[]   NYI   = new int  [NADMA  + 1]; // Kronecker/structure indices
    public int[]   IAD   = new int  [NADMA  + 1]; // output-channel mapping
    public int[]   IDA   = new int  [NADDAX + 1]; // input-channel mapping
    public float[] W     = new float[NPWMA  + 1]; // model parameter vector

    public AppState()
    {
        // Defaults from original BLOCK DATA initialization
        NRDM[1] = 11; NRDM[2] = 11;
        NSEQ[1] = 1;  NSEQ[2] = 1;
        NRSB[1] = 7;  NRSB[2] = 7;
        IPL[1]  = 1;
        NPL     = 1;
        for (int i = 1; i <= NADMA;  i++) ISCALE[i] = 1;
        for (int i = 1; i <= NADDAX; i++)
        {
            TEXT[i] = new string(' ', 20);
            PBEZ[i] = "      ";
        }
    }
}
