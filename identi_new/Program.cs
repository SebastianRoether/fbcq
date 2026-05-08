namespace Identi;

// ============================================================
// Program – main menu-driven application entry point.
//
// Faithful translation of IDENTI.F77 (Roether, 1986) and
// XIDEN.F77 (identification sub-menu).
//
// Hardware-dependent sections (A/D converter, D/A converter,
// interrupt service routine from OFFASM.A86 / ONLASM.A86)
// are replaced by simulation stubs.  All menu numbering and
// sub-menu structure follows the original exactly.
// ============================================================

internal static class Program
{
    static readonly AppState st = new();

    // Fixed matrix dimensions for the identification work arrays
    private const int LTH   = 30;
    private const int LTH2  = 61;
    private const int LTHLM = 38;
    private const int LM    = 8;
    private const int LU    = 10;

    // Identification work arrays (allocate once)
    static readonly float[,] dmat  = new float[LTH   + 1, LTHLM + 1];
    static readonly float[,] rh    = new float[LTH   + 1, LTH   + 1];
    static readonly float[,] bWork = new float[LTH   + 1, LU    + 1];
    static readonly int[,]   ipos0 = new int  [LTH   + 1, LM    + 1];
    static readonly float[,] wksp  = new float[LTH   + 1, LTH2  + 1];

    // graphic state
    static int  _lengr  = 0;
    static int  _izgr   = 0;
    static bool _ibiset = false;

    // ============================================================
    static void Main(string[] args)
    {
        Console.OutputEncoding = System.Text.Encoding.UTF8;
        Console.WriteLine();
        Console.WriteLine("  ╔══════════════════════════════════════════════════╗");
        Console.WriteLine("  ║  IDENTI – Process Parameter Identification       ║");
        Console.WriteLine("  ║  C# port of F. Roether, 1986  (v1.0, 2026)      ║");
        Console.WriteLine("  ╚══════════════════════════════════════════════════╝");

        MainMenu();
    }

    // ============================================================
    // MAIN MENU  (original Fortran label 100)
    // ============================================================
    static void MainMenu()
    {
        while (true)
        {
            Console.WriteLine();
            Console.WriteLine("  Process Parameter Identification – Main Menu");
            Console.WriteLine();
            Console.WriteLine("  Exit program ________________________________  1");
            Console.WriteLine("  Set configuration parameters ________________  2");
            Console.WriteLine("  Display configuration parameters ____________  3");
            Console.WriteLine("  Disk data communication _____________________  4");
            Console.WriteLine("  Simulate data acquisition (offline) _________  5");
            Console.WriteLine("  Stop data acquisition _______________________  6");
            Console.WriteLine("  Cyclic channel value display ________________  7");
            Console.WriteLine("  Graphic channel display ______________________  8");
            Console.WriteLine("  System identification ________________________  9");
            Console.WriteLine("  Auto test: PT4 step response (T=10s, K=1, Ts=1s)  a");
            Console.WriteLine("  Auto test: PT1 PRBS identification (T=5s, K=1, Ts=1s)  b");
            Console.WriteLine();
            string job = ReadMainMenuChoice();

            switch (job)
            {
                case "1": return;              // exit
                case "2": ConfigMenu();  break;
                case "3": PrintConfig(); break;
                case "4": DiskMenu();    break;
                case "5": SimulateAcquisition(); break;
                case "6": StopAcquisition();     break;
                case "7": CyclicDisplay();       break;
                case "8": GraphMenu();           break;
                case "9": IdentMenu();           break;
                case "a": RunAutoTest();         break;
                case "b": RunAutoTestPRBS();     break;
            }
        }
    }

    // ============================================================
    // 2 – CONFIGURATION SUB-MENU  (Fortran label 2000)
    // ============================================================
    static void ConfigMenu()
    {
        while (true)
        {
            Console.WriteLine();
            Console.WriteLine("  Configuration parameters – choices 2-6 reset the data pointer:");
            Console.WriteLine();
            Console.WriteLine("  Return to main menu ______________________________  1");
            Console.WriteLine("  Number of A/D channels ___________________________  2");
            Console.WriteLine("  Number of D/A channels ___________________________  3");
            Console.WriteLine("  Clock interrupt and multiplier ___________________  4");
            Console.WriteLine("  DA channel excitation signals ____________________  5");
            Console.WriteLine("  Data buffer length _______________________________  6");
            Console.WriteLine("  Mean-value computation mode ______________________  7");
            Console.WriteLine("  Channel text labels ______________________________  8");
            Console.WriteLine("  Physical measurement ranges ______________________  9");
            Console.WriteLine();
            int job2 = ReadInt("  Your choice: ", 1, 9);

            if (job2 == 1) return;

            switch (job2)
            {
                case 2:
                    st.NADU = ReadInt("  Number of A/D channels: ", 1, st.NADMAX);
                    ResetGraphPointers(); break;
                case 3:
                    st.NDAU = ReadInt("  Number of D/A channels: ", 1, st.NDAMAX);
                    ResetGraphPointers(); break;
                case 4:
                    st.ITIME  = ReadIntChoice("  Clock interrupt (1, 10, or 600 × 100ms): ",
                                              new[]{1,10,600});
                    st.ICNEND = ReadInt("  Multiplier (product = sample time): ", 1, 1000);
                    break;
                case 5:
                    DefineExcitationSignal(); break;
                case 6:
                    st.LEN = ReadInt("  Data buffer length: ", 1, AppState.LENMA);
                    ResetGraphPointers(); break;
                case 7:
                    MeanValueMode(); break;
                case 8:
                    SetChannelLabels(); break;
                case 9:
                    SetPhysicalRanges(); break;
            }
        }
    }

    // ---- 2-5: D/A excitation signal definition ----
    static void DefineExcitationSignal()
    {
        if (st.NDAU <= 0) { Console.WriteLine(" *No D/A channels defined.*"); return; }
        int ic = ReadInt("  Which D/A channel? (1-based): ", 1, st.NDAU);

        Console.WriteLine($"  Signal type (length {st.LEN})?");
        Console.WriteLine("  Rectangle  ________________________________________________  1");
        Console.WriteLine("  Pseudo-binary noise (PRBS) ________________________________  2");
        Console.WriteLine("  User-defined piecewise-linear signal ______________________  3");
        int job21 = ReadInt("  Your choice: ", 1, 3);

        int ic1 = ic + st.NADMAX;
        if (job21 == 3) { DefineCustomSignal(ic1); return; }

        float vm   = 10f * st.IMW[ic1] / (float)(st.IDAMAX - st.IDAMIN);
        vm = ReadFloat("  Mean value in Volts: ", vm);
        int idamw  = (int)(vm / 10f * (st.IDAMAX - st.IDAMIN));
        st.IMW[ic1] = idamw;

        float va   = 10f * st.IVMA[ic] / (float)(st.IDAMAX - st.IDAMIN);
        va = ReadFloat("  Amplitude in Volts: ", va);
        int idamv  = (int)(va / 10f * (st.IDAMAX - st.IDAMIN));
        st.IVMA[ic] = idamv;

        int idami = idamw - idamv;
        int idama = idamw + idamv;
        st.NSEQ[ic] = ReadInt("  Sequence length (× sample time): ", 1, st.LEN);

        if (job21 == 1)
            GenerateRectangle(ic1, idami, idama, st.NSEQ[ic]);
        else
            GeneratePRBS(ic1, ic, idami, idama, st.NRDM[ic], st.NRSB[ic], st.NSEQ[ic]);
    }

    static void GenerateRectangle(int ch, int lo, int hi, int seqLen)
    {
        st.NRDM[ch - st.NADMAX] = 0;
        st.NRSB[ch - st.NADMAX] = 0;
        st.IDAT[ch, 1] = (short)hi;
        for (int i = 2; i <= st.LEN; i++)
        {
            if (seqLen == 1 || (i % seqLen) == 1)
                st.IDAT[ch, i] = st.IDAT[ch, i - 1] == (short)hi ? (short)lo : (short)hi;
            else
                st.IDAT[ch, i] = st.IDAT[ch, i - 1];
        }
        Console.WriteLine("  Rectangle signal generated.");
    }

    static void GeneratePRBS(int daIdx, int ic, int lo, int hi, int regLen, int fbIdx, int seqLen)
    {
        regLen = ReadInt("  Shift-register length: ", 2, 20);
        st.NRDM[ic] = regLen;
        fbIdx = ReadInt("  Feedback register index: ", 1, regLen);
        st.NRSB[ic] = fbIdx;

        int[] reg = new int[regLen + 1];
        for (int i = 1; i <= regLen; i++) reg[i] = lo;
        reg[1] = hi; reg[5 <= regLen ? 5 : regLen] = hi;
        if (9 <= regLen) reg[9] = hi;
        if (10 <= regLen) reg[10] = hi;

        int idamw2 = lo + hi; // 2 × mean = lo+hi
        for (int i = 1; i <= st.LEN; i++)
        {
            st.IDAT[daIdx, i] = (short)reg[regLen];
            if (seqLen == 1 || (i % seqLen) == 1)
            {
                int n1 = reg[fbIdx] + reg[regLen] != idamw2 ? hi : lo;
                for (int k = regLen; k > 1; k--) reg[k] = reg[k - 1];
                reg[1] = n1;
            }
        }
        Console.WriteLine("  PRBS signal generated.");
    }

    static void DefineCustomSignal(int daIdx)
    {
        int istart = 1, iwold = st.IDAMIN;
        while (true)
        {
            int iend = ReadInt("  Sample time point (0 to finish): ", 0, st.LEN);
            if (iend == 0) break;
            float w  = ReadFloat("  Value in Volts: ", 10f * iwold / (st.IDAMAX - st.IDAMIN));
            int iwert = (int)(w / 10f * (st.IDAMAX - st.IDAMIN));
            int iabst = iend - istart;
            if (iabst < 0) iabst = 1;
            for (int i = istart; i <= Math.Min(iend, st.LEN); i++)
            {
                float slope = iabst > 0 ? (float)(iwert - iwold) / iabst : 0f;
                st.IDAT[daIdx, i] = (short)(iwold + (int)(slope * (i - istart)));
            }
            istart = iend; iwold = iwert;
            if (iend >= st.LEN) break;
        }
    }

    // ---- 2-7: mean value mode ----
    static void MeanValueMode()
    {
        Console.WriteLine();
        Console.WriteLine("  Mean-value computation:");
        Console.WriteLine("  No mean subtraction ___________________________  0");
        Console.WriteLine("  Explicit mean values __________________________  1");
        Console.WriteLine("  Compute from measurement history ______________  2");
        Console.WriteLine("  Online mean (reset) ___________________________  3");
        int itypmw = ReadInt("  Your choice: ", 0, 3);
        st.ITYPMW = itypmw;

        switch (itypmw)
        {
            case 0:
                for (int i = 1; i <= st.NADU; i++) st.IMW[i] = st.IADMIN;
                break;
            case 1:
                for (int i = 1; i <= st.NADU; i++)
                {
                    Console.Write($"  A/D channel {i}");
                    float v = 10f * st.IMW[i] / (float)(st.IADMAX - st.IADMIN);
                    float nv = ReadFloat("  Mean value in Volts: ", v);
                    st.IMW[i] = (int)(nv / 10f * (st.IADMAX - st.IADMIN));
                }
                break;
            case 2:
                int imwlag = ReadInt("  How many points from history? ", 1, st.LEN);
                int npsave2 = st.NPOINT - imwlag;
                for (int j = 1; j <= st.NADU; j++)
                {
                    st.IMW[j] = 0;
                    for (int i = 1; i <= imwlag; i++)
                        st.IMW[j] += st.IDAT[j, Identification.Iplag(npsave2, i, st.LEN)];
                    st.IMW[j] /= imwlag;
                }
                Console.WriteLine("  Means computed from history.");
                break;
            case 3:
                for (int i = 1; i <= st.NADU; i++) st.IMW[i] = 0;
                st.IZMOLD = st.IZ;
                Console.WriteLine("  Online mean computation reset.");
                break;
        }
    }

    // ---- 2-8: channel labels ----
    static void SetChannelLabels()
    {
        int nadda = st.NADU + st.NDAU;
        if (nadda <= 0) { Console.WriteLine(" *No channels defined.*"); return; }
        for (int i = 1; i <= st.NADU; i++)
        {
            Console.WriteLine($"  A/D channel {i}");
            Console.WriteLine($"  Current title: {st.TEXT[i]}");
            Console.Write("  New title (max 20 chars): ");
            string? t = Console.ReadLine();
            if (t != null) st.TEXT[i] = t.PadRight(20)[..20];
            Console.WriteLine($"  Physical unit (current: {st.PBEZ[i]?.Trim()}):");
            Console.Write("  New unit (max 6 chars): ");
            string? u = Console.ReadLine();
            if (u != null) st.PBEZ[i] = u.PadRight(6)[..6];
        }
        for (int i = 1; i <= st.NDAU; i++)
        {
            int ch = st.NADMAX + i;
            Console.WriteLine($"  D/A channel {i}");
            Console.WriteLine($"  Current title: {st.TEXT[ch]}");
            Console.Write("  New title (max 20 chars): ");
            string? t = Console.ReadLine();
            if (t != null) st.TEXT[ch] = t.PadRight(20)[..20];
            Console.WriteLine($"  Physical unit (current: {st.PBEZ[ch]?.Trim()}):");
            Console.Write("  New unit (max 6 chars): ");
            string? u = Console.ReadLine();
            if (u != null) st.PBEZ[ch] = u.PadRight(6)[..6];
        }
    }

    // ---- 2-9: physical ranges ----
    static void SetPhysicalRanges()
    {
        int nadda = st.NADU + st.NDAU;
        for (int i = 1; i <= nadda; i++)
        {
            Console.WriteLine($"  Channel {i}:");
            st.PMIN[i] = ReadFloat("    Minimum: ", st.PMIN[i]);
            st.PMAX[i] = ReadFloat("    Maximum: ", st.PMAX[i]);
        }
    }

    // ---- reset graph state after channel-count change ----
    static void ResetGraphPointers()
    {
        int nadda = st.NADU + st.NDAU;
        st.NPL = nadda;
        for (int i = 1; i <= st.NPL; i++)
            st.IPL[i] = (short)(i <= st.NADU ? i : st.NADMAX + i - st.NADU);
    }

    // ============================================================
    // 3 – DISPLAY CONFIGURATION  (Fortran label 3000)
    // ============================================================
    static void PrintConfig()
    {
        float tsamp = st.ITIME * st.ICNEND * 100f; // ms
        Console.WriteLine();
        Console.WriteLine("  Configuration:");
        Console.WriteLine($"    A/D channels:       {st.NADU}");
        Console.WriteLine($"    D/A channels:       {st.NDAU}");
        Console.WriteLine($"    Data length:        {st.LEN}");
        Console.WriteLine($"    Sample time:        {tsamp} ms");
        Console.WriteLine($"    Mean mode:          {st.ITYPMW}");
        Console.WriteLine($"    Filter parameter:   {st.FPAR}");
        Console.WriteLine($"    Plot channels:      {st.NPL}");
        Console.Write("    Plotted channels:  ");
        for (int i = 1; i <= st.NPL; i++) Console.Write($" {st.IPL[i]}");
        Console.WriteLine();
        Console.WriteLine($"    A/D range:          {st.IADMIN} .. {st.IADMAX}");
        Console.WriteLine($"    D/A range:          {st.IDAMIN} .. {st.IDAMAX}");
        Console.WriteLine($"    Buffer pointer:     {st.NPOINT} / {st.LEN}");
        Console.WriteLine($"    Online flag:        {(st.IONLFL == 1 ? "running" : "stopped")}");
    }

    // ============================================================
    // 4 – DISK COMMUNICATION SUB-MENU  (Fortran label 4000)
    // ============================================================
    static void DiskMenu()
    {
        while (true)
        {
            Console.WriteLine();
            Console.WriteLine("  Disk communication:");
            Console.WriteLine("  Return to main menu _____________________________  0");
            Console.WriteLine("  Load configuration from disk ____________________  1");
            Console.WriteLine("  Load measurement and config from disk ____________  2");
            Console.WriteLine("  Save configuration to disk ______________________  3");
            Console.WriteLine("  Save measurement and config to disk ______________  4");
            Console.WriteLine("  Import Honeywell H-TMS-3000 file ________________  5");
            Console.WriteLine();
            int job41 = ReadInt("  Your choice: ", 0, 5);
            if (job41 == 0) return;

            Console.Write("  Enter filename: ");
            string? fname = Console.ReadLine()?.Trim();
            if (string.IsNullOrEmpty(fname)) continue;

            switch (job41)
            {
                case 1:
                    if (DiskIo.LoadConfig(fname, st))
                    { Console.WriteLine("  Configuration loaded."); st.IONLFL = 0; }
                    break;
                case 2:
                    if (DiskIo.LoadData(fname, st))
                        Console.WriteLine("  Data loaded.");
                    break;
                case 3:
                    if (DiskIo.SaveConfig(fname, st))
                        Console.WriteLine("  Configuration saved.");
                    break;
                case 4:
                    if (DiskIo.SaveData(fname, st))
                        Console.WriteLine("  Data saved.");
                    break;
                case 5:
                    ImportHtms(fname); break;
            }
        }
    }

    static void ImportHtms(string fname)
    {
        int ch   = ReadInt("  Target channel: ", 1, AppState.NADDAX);
        st.PMIN[ch] = ReadFloat("  Physical minimum: ", st.PMIN[ch]);
        st.PMAX[ch] = ReadFloat("  Physical maximum: ", st.PMAX[ch]);
        int scale  = ReadInt("  Scale factor (positive = multiply, negative = divide): ",
                             -100, 100);
        int shift  = ReadInt("  Row offset (skip N rows): ", 0, 1000);
        int decim  = ReadInt("  Decimation factor (1 = none): ", 1, 10);
        if (DiskIo.ImportHtms(fname, st, ch, st.PMIN[ch], st.PMAX[ch], scale, shift, decim))
            Console.WriteLine($"  H-TMS data imported into channel {ch}.");
    }

    // ============================================================
    // 5 – SIMULATE ACQUISITION  (replaces hardware start, Fortran 5000)
    // ============================================================
    static void SimulateAcquisition()
    {
        Console.WriteLine();
        Console.WriteLine("  Hardware A/D acquisition is not available in this platform port.");
        Console.WriteLine("  You can:");
        Console.WriteLine("  [1] Load data from a file (use disk menu).");
        Console.WriteLine("  [2] Enter measurement values manually.");
        Console.WriteLine("  [0] Cancel.");
        int c = ReadInt("  Choice: ", 0, 2);
        if (c == 2) ManualDataEntry();
    }

    static void ManualDataEntry()
    {
        Console.WriteLine($"  Enter {st.LEN} rows with {st.NADU} A/D and {st.NDAU} D/A values.");
        Console.WriteLine("  Format per row: v1 v2 ... (in Volts, press Enter after each row)");
        Console.WriteLine("  Type 'done' to finish early.");
        int nadda = st.NADU + st.NDAU;
        for (int i = 1; i <= st.LEN; i++)
        {
            Console.Write($"  Row {i,4}: ");
            string? line = Console.ReadLine();
            if (line == null || line.Trim().Equals("done", StringComparison.OrdinalIgnoreCase))
            { st.LEN = i - 1; break; }
            var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            for (int j = 1; j <= Math.Min(nadda, parts.Length); j++)
            {
                if (float.TryParse(parts[j - 1], out float v))
                {
                    bool isAD = j <= st.NADU;
                    float range = isAD ? (st.IADMAX - st.IADMIN) : (st.IDAMAX - st.IDAMIN);
                    float plo   = st.PMIN[j], phi2 = st.PMAX[j];
                    short raw   = (plo != phi2)
                        ? (short)Math.Round((v - plo) / (phi2 - plo) * range)
                        : (short)Math.Round(v);
                    st.IDAT[j, i] = raw;
                }
            }
        }
        st.NPOINT = st.LEN;
        st.IZ     = st.LEN;
        Console.WriteLine($"  {st.LEN} data rows entered.");
    }

    // ============================================================
    // 6 – STOP ACQUISITION  (Fortran label 6000)
    // ============================================================
    static void StopAcquisition()
    {
        st.IONLFL = 0;
        Console.WriteLine("  Acquisition stopped.");
    }

    // ============================================================
    // 7 – CYCLIC CHANNEL DISPLAY  (Fortran label 7000)
    // ============================================================
    static void CyclicDisplay()
    {
        Console.WriteLine("  Press Enter to refresh, 'q' + Enter to quit.");
        while (true)
        {
            ConsoleGraph.PrintChannelValues(st);
            Console.Write("  [Enter=refresh, q=quit]: ");
            string? key = Console.ReadLine();
            if (key?.Trim().Equals("q", StringComparison.OrdinalIgnoreCase) == true)
                break;
        }
    }

    // ============================================================
    // 8 – GRAPHICS SUB-MENU  (Fortran XGRAPH label 100)
    // ============================================================
    static void GraphMenu()
    {
        if (!_ibiset) { _lengr = st.LEN; _izgr = 0; }

        while (true)
        {
            Console.WriteLine();
            Console.WriteLine("  Graph display:");
            Console.WriteLine("  Return to main menu ___________________________________  1");
            Console.WriteLine("  Redefine plotted channels _____________________________  2");
            Console.WriteLine("  Plot data contents ____________________________________  3");
            Console.WriteLine("  Plot data with model simulation ______________________  4");
            Console.WriteLine("  Online channel display (last acquired) ________________  5");
            Console.WriteLine();
            int job = ReadInt("  Your choice: ", 1, 5);
            if (job == 1) return;

            switch (job)
            {
                case 2: RedefineChannels();             break;
                case 3: ConsoleGraph.PlotChannels(st, _lengr, _izgr, false); break;
                case 4:
                    if (st.JOBID <= 0)
                    { Console.WriteLine(" *No model identified yet.*"); break; }
                    ConsoleGraph.PlotChannels(st, _lengr, _izgr, true);
                    break;
                case 5: ConsoleGraph.PlotChannels(st, st.LEN, 0, false); break;
            }
        }
    }

    static void RedefineChannels()
    {
        Console.WriteLine("  Define which A/D channels to plot (0 to skip):");
        int npad = ReadInt($"  How many A/D channels (0-{st.NADU})? ", 0, st.NADU);
        if (npad > 0)
        {
            Console.Write("  Channel numbers: ");
            string? s = Console.ReadLine();
            var nums = s?.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            for (int i = 1; i <= npad && nums != null && i - 1 < nums.Length; i++)
                if (int.TryParse(nums[i - 1], out int ch)) st.IPL[i] = ch;
        }
        int npda = ReadInt($"  How many D/A channels (0-{st.NDAU})? ", 0, st.NDAU);
        if (npda > 0)
        {
            Console.Write("  Channel numbers: ");
            string? s = Console.ReadLine();
            var nums = s?.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            for (int i = 1; i <= npda && nums != null && i - 1 < nums.Length; i++)
                if (int.TryParse(nums[i - 1], out int ch))
                    st.IPL[npad + i] = st.NADMAX + ch;
        }
        st.NPL = npad + npda;

        // filter parameters
        Console.WriteLine("  Filter type:");
        Console.WriteLine("  0 = none  1 = subtract mean  2 = differentiate  3 = high-pass");
        st.JOBPL = ReadInt("  Choice: ", 0, 3);
        if (st.JOBPL == 3)
        {
            float tfilt = -(float)(st.ITIME * st.ICNEND) / (10f * MathF.Log(st.FPAR));
            tfilt = ReadFloat("  Filter time constant (seconds): ", tfilt);
            st.FPAR = MathF.Exp((float)(st.ITIME * st.ICNEND) / (-10f * tfilt));
        }

        // view window
        _lengr = ReadInt("  View length (samples): ", 1, st.LEN);
        _izgr  = ReadInt("  Forward offset (samples): ", 0, st.LEN - 1);
        _ibiset = true;
    }

    // ============================================================
    // 9 – IDENTIFICATION SUB-MENU  (Fortran XIDEN label 100)
    // ============================================================
    static void IdentMenu()
    {
        while (true)
        {
            Console.WriteLine();
            Console.WriteLine("  Process identification sub-menu:");
            Console.WriteLine();
            Console.WriteLine("  Return to main menu _______________________________  0");
            Console.WriteLine("  Channel assignment for system variables ___________  1");
            Console.WriteLine("  Set number of estimation samples __________________  2");
            Console.WriteLine("  Define known decoupling constraints _______________  3");
            Console.WriteLine("  Enable/disable bias estimation ___________________  4");
            Console.WriteLine("  Run: Standard Least Squares ______________________  5");
            Console.WriteLine("  Run: QR Least Squares ____________________________  6");
            Console.WriteLine("  Run: Standard Instrumental Variables _____________  7");
            Console.WriteLine("  Run: QR Instrumental Variables __________________  8");
            Console.WriteLine("  Print model (state-space) to console _____________  9");
            Console.WriteLine("  Print model to file ______________________________  10");
            Console.WriteLine();
            int job = ReadInt("  Your choice: ", 0, 10);
            if (job == 0) return;

            switch (job)
            {
                case 1: AssignSystemChannels(); break;
                case 2:
                    st.NANF = ReadInt("  Estimation start offset: ", 1, st.LEN);
                    st.IANZ = ReadInt("  Number of estimation samples: ", 1, st.LEN);
                    break;
                case 3: DefineConstraints(); break;
                case 4:
                    st.IGLW = ReadInt("  Bias estimation: 0=off, 1=on: ", 0, 1);
                    break;
                case 5: RunIdentification(1); break;
                case 6: RunIdentification(2); break;
                case 7: RunIdentification(3); break;
                case 8: RunIdentification(4); break;
                case 9: Identification.PrintModel(st, false); break;
                case 10:
                    Console.Write("  Filename: ");
                    string? fn = Console.ReadLine()?.Trim();
                    if (!string.IsNullOrEmpty(fn)) SaveModelToFile(fn); break;
            }
        }
    }

    static void AssignSystemChannels()
    {
        st.NM = ReadInt("  Number of system outputs: ", 1, AppState.NADMA);
        st.NU = ReadInt("  Number of system inputs:  ", 1, AppState.NADMA);
        for (int i = 1; i <= st.NM; i++)
        {
            Console.WriteLine($"  Which channel is system output #{i}?");
            st.IAD[i] = (short)ReadInt("    Channel: ", 1, AppState.NADDAX);
            Console.WriteLine($"  Structure index for output #{i}?");
            st.NYI[i] = (short)ReadInt("    Index: ", 1, 10);
        }
        for (int i = 1; i <= st.NU; i++)
        {
            Console.WriteLine($"  Which channel is system input #{i}?");
            st.IDA[i] = (short)ReadInt("    Channel: ", 1, AppState.NADDAX);
        }
        // clear constraint map
        for (int i = 1; i <= LTH; i++)
            for (int j = 1; j <= LM; j++)
                ipos0[i, j] = 0;
        Console.WriteLine("  Channel assignment done.");
    }

    static void DefineConstraints()
    {
        Console.WriteLine("  Enter known zero constraints (row, col of parameter matrix).");
        Console.WriteLine("  Row 0 to finish.");
        while (true)
        {
            int row = ReadInt("  Row (0 to finish): ", 0, LTH);
            if (row == 0) break;
            int col = ReadInt("  Column: ", 1, LM);
            ipos0[row, col] = 1;
            Console.WriteLine($"  Constraint set: ipos0[{row},{col}] = 1");
        }
    }

    static void RunIdentification(int method)
    {
        if (st.NPOINT == 0) { Console.WriteLine(" *No data available.*"); return; }
        if (st.NM <= 0 || st.NU <= 0)
        { Console.WriteLine(" *Set channel assignment first (option 1).*"); return; }

        // compute N = Σ NYI
        int n2 = 0;
        for (int i = 1; i <= st.NM; i++) n2 += st.NYI[i];
        if (n2 == 0) { Console.WriteLine(" *Define structure indices first.*"); return; }
        st.N = n2;

        string[] names = { "","Least Squares","QR-Least Squares",
                            "Instrumental Variables","QR-Instrumental Variables" };
        Console.WriteLine($"\n  Running {names[method]}...");

        int job = method;
        Identification.Xident(
            st.W, st.IDAT,
            st.IDA, st.IAD,
            st.LEN, st.NANF, st.IANZ, st.NPOINT,
            st.NYI, st.NU, st.NM,
            dmat, rh, bWork, ipos0, st.IGLW, wksp,
            ref job);

        switch (job)
        {
            case 0:
                st.JOBID = method;
                Console.WriteLine("  Identification successful.");
                Identification.PrintModel(st);
                break;
            case 1:
                Console.WriteLine(" *Error: Data matrix singular – result unusable.*");
                break;
            case 2:
                Console.WriteLine(" *Error: Unstable auxiliary model – result unusable.*");
                break;
        }
    }

    // ============================================================
    // AUTO TEST – sine mock data, default config, LS identification
    // ============================================================
    static void RunAutoTest()
    {
        // PT4 = 4 cascaded PT1 blocks, T=10s, K=1, Ts=1s
        // ZOH-discretised PT1: y[k] = a·y[k-1] + b·u[k-1]
        //   a = e^(−Ts/T) = e^(−0.1) ≈ 0.904837
        //   b = 1 − a      ≈ 0.095163
        // All 4 poles at z = a.  DC gain = 1.
        const int    len    = 400;           // 400 samples = 400 s
        const int    stepAt = 50;            // step at t = 50 s
        const double a      = 0.904837418036; // e^(−0.1)
        const double b      = 1.0 - a;

        // ---- configuration ----
        st.NADU   = 1;
        st.NDAU   = 1;
        st.LEN    = len;
        st.NM     = 1;
        st.NU     = 1;
        st.NYI[1] = 4;                  // 4th-order ARX model
        st.N      = 4;
        st.IAD[1] = 1;                  // output → A/D channel 1
        st.IDA[1] = st.NADMAX + 1;     // input  → D/A channel 1  (index 9)
        st.NANF   = 5;
        st.IANZ   = len - 5;
        st.IGLW   = 0;
        st.JOBID  = 0;

        int chIn  = st.NADMAX + 1;     // = 9
        int chOut = 1;

        // physical 0..1 → raw 0..4096 (= IADMAX)
        const short rawOne = 4096;
        st.PMIN[chOut] = 0f; st.PMAX[chOut] = 1f;
        st.PMIN[chIn]  = 0f; st.PMAX[chIn]  = 1f;
        st.TEXT[chOut] = "Output (PT4)        ";
        st.TEXT[chIn]  = "Input  (step)       ";
        st.PBEZ[chOut] = "[-]   ";
        st.PBEZ[chIn]  = "[-]   ";

        // ---- step input: 0 for t<50s, 1 for t≥50s ----
        for (int k = 1; k <= len; k++)
            st.IDAT[chIn, k] = k >= stepAt ? rawOne : (short)0;

        // ---- simulate 4 cascaded ZOH PT1 stages ----
        double x1 = 0, x2 = 0, x3 = 0, x4 = 0, uPrev = 0;
        for (int k = 1; k <= len; k++)
        {
            double uCurr = st.IDAT[chIn, k];
            double nx1 = a * x1 + b * uPrev;
            double nx2 = a * x2 + b * x1;
            double nx3 = a * x3 + b * x2;
            double nx4 = a * x4 + b * x3;
            x1 = nx1; x2 = nx2; x3 = nx3; x4 = nx4;
            st.IDAT[chOut, k] = (short)Math.Clamp((int)Math.Round(x4), -32000, 32000);
            uPrev = uCurr;
        }

        st.NPOINT = len;
        st.IZ     = len;

        float b4 = (float)(b * b * b * b);
        Console.WriteLine();
        Console.WriteLine("  [AUTO TEST]  PT4 step response");
        Console.WriteLine($"    Model   : 4 cascaded PT1,  T=10s,  K=1,  Ts=1s");
        Console.WriteLine($"    Pole    : z = e^(−0.1) ≈ {a:F6}  (all 4 identical)");
        Console.WriteLine($"    Input   : step 0→1 at t={stepAt}s");
        Console.WriteLine($"    True AR : W[4]=4a≈{4*a:F4}  W[3]=−6a²≈{-6*a*a:F4}  W[2]=4a³≈{4*a*a*a:F4}  W[1]=−a⁴≈{-a*a*a*a:F4}");
        Console.WriteLine($"    True MA : W[5]=b⁴≈{b4:G4} at u[k-4]   (W[6..8]=0)");
        Console.WriteLine($"    DC gain = 1   (steady-state output = input)");
        Console.WriteLine($"    Samples : {len} = {len}s");
        Console.WriteLine();

        // ---- zero constraint map ----
        for (int i = 1; i <= LTH; i++)
            for (int j = 1; j <= LM; j++)
                ipos0[i, j] = 0;

        // ---- Least-Squares identification ----
        // Note: a pure step has limited spectral content; the LS result will
        // approximate the AR coefficients but MA precision will be low.
        Console.WriteLine("  Running Least-Squares identification...");
        int job = 1;
        Identification.Xident(
            st.W, st.IDAT,
            st.IDA, st.IAD,
            st.LEN, st.NANF, st.IANZ, st.NPOINT,
            st.NYI, st.NU, st.NM,
            dmat, rh, bWork, ipos0, st.IGLW, wksp,
            ref job);

        switch (job)
        {
            case 0:
                st.JOBID = 1;
                Console.WriteLine("  Identification successful.");
                Identification.PrintModel(st);
                CheckStability();
                break;
            case 1:
                Console.WriteLine(" *Singular matrix – step input lacks spectral richness for LS.*");
                break;
            case 2:
                Console.WriteLine(" *Error: Unstable auxiliary model.*");
                break;
        }

        // ---- plot both channels ----
        st.IPL[1] = chOut;
        st.IPL[2] = chIn;
        st.NPL    = 2;
        st.JOBPL  = 0;
        ConsoleGraph.PlotChannels(st, len, 0, false);
    }

    // ============================================================
    // AUTO TEST B – first-order PT1 with 7-bit PRBS input
    // Demonstrates AR coefficient recovery via backward LS.
    // ============================================================
    static void RunAutoTestPRBS()
    {
        // Two cascaded PT1s (2nd-order):
        //   x[k] = z1·x[k-1] + (1-z1)·u[k-1]     (inner state, T1=5s)
        //   y[k] = z2·y[k-1] + (1-z2)·x[k-1]      (output,     T2=10s)
        //
        // Equivalent AR(2) difference equation:
        //   y[k] = a1·y[k-1] + a2·y[k-2] + b·u[k-2]
        //   a1 = z1+z2,  a2 = −z1·z2,  b = (1−z1)·(1−z2),  DC gain = 1
        //
        // Backward Xident (n=2) recovers via Yule-Walker symmetry:
        //   W[1] ≈ a2  (coefficient of y[t+2])
        //   W[2] ≈ a1  (coefficient of y[t+1])
        //   W[3], W[4] ≈ 0  (backward causality kills MA terms)
        const double z1 = 0.818730753078; // e^{-0.2}, T1=5s
        const double z2 = 0.904837418036; // e^{-0.1}, T2=10s
        const double a1 = z1 + z2;        // ≈ 1.723568
        const double a2 = -(z1 * z2);     // ≈ −0.740818 (= −e^{−0.3})
        const double b  = (1-z1) * (1-z2);// ≈ 0.017245, DC gain = 1
        const int    len = 400;
        const short  hi  = 2048;          // PRBS amplitude (raw counts)
        const short  lo  = -2048;

        // ---- configuration ----
        st.NADU   = 1;
        st.NDAU   = 1;
        st.LEN    = len;
        st.NM     = 1;
        st.NU     = 1;
        st.NYI[1] = 2;                  // 2nd-order model
        st.N      = 2;
        st.IAD[1] = 1;                  // output → A/D ch 1
        st.IDA[1] = st.NADMAX + 1;      // input  → D/A ch 1 (index 9)
        st.NANF   = 3;                  // need 2 future points → start at sample 3
        st.IANZ   = len - 3;
        st.IGLW   = 0;
        st.JOBID  = 0;

        int chIn  = st.NADMAX + 1;  // = 9
        int chOut = 1;

        st.PMIN[chOut] = -15f; st.PMAX[chOut] = 15f;
        st.PMIN[chIn]  = -1f;  st.PMAX[chIn]  = 1f;
        st.TEXT[chOut] = "Output (2xPT1 casc.)";
        st.TEXT[chIn]  = "Input  (7-bit PRBS) ";
        st.PBEZ[chOut] = "[V]   ";
        st.PBEZ[chIn]  = "[V]   ";

        // ---- 7-bit maximal PRBS (poly x^7+x^6+1, period=127) ----
        // 400 samples ≈ 3.15 periods → persistently exciting of order 2.
        int[] reg = new int[8];
        for (int i = 1; i <= 7; i++) reg[i] = 1;  // all-ones seed
        for (int k = 1; k <= len; k++)
        {
            st.IDAT[chIn, k] = reg[7] == 1 ? hi : lo;
            int fb = reg[7] ^ reg[6];              // feedback taps 7 and 6
            for (int r = 7; r > 1; r--) reg[r] = reg[r - 1];
            reg[1] = fb;
        }

        // ---- simulate: cascade state-space ----
        double xPrev = 0, yPrev = 0, uPrev = 0;
        for (int k = 1; k <= len; k++)
        {
            double xCurr = z1 * xPrev + (1-z1) * uPrev;
            double yCurr = z2 * yPrev + (1-z2) * xPrev;
            st.IDAT[chOut, k] = (short)Math.Clamp((long)Math.Round(yCurr), -32000, 32000);
            xPrev = xCurr;
            yPrev = yCurr;
            uPrev = st.IDAT[chIn, k];
        }

        st.NPOINT = len;
        st.IZ     = len;

        Console.WriteLine();
        Console.WriteLine("  [AUTO TEST B]  2nd-order cascaded PT1 with 7-bit PRBS identification");
        Console.WriteLine($"    System  : PT1(T=5s) → PT1(T=10s),  K=1,  Ts=1s");
        Console.WriteLine($"    Poles   : z1=e^(−0.2)≈{z1:F6},  z2=e^(−0.1)≈{z2:F6}");
        Console.WriteLine($"    AR coefs: a1=z1+z2≈{a1:F6},  a2=−z1·z2≈{a2:F6}");
        Console.WriteLine($"    Input   : 7-bit PRBS (period=127, {len/127.0:F1} periods in {len} samples)");
        Console.WriteLine($"    Backward Xident (n=2) should recover via Yule-Walker:");
        Console.WriteLine($"      W[1] ≈ a2 ≈ {a2:F6}  (coeff of y[t+2])");
        Console.WriteLine($"      W[2] ≈ a1 ≈ {a1:F6}  (coeff of y[t+1])");
        Console.WriteLine($"      W[3],W[4] ≈ 0         (backward causality)");
        Console.WriteLine();

        // ---- zero constraint map ----
        for (int i = 1; i <= LTH; i++)
            for (int j = 1; j <= LM; j++)
                ipos0[i, j] = 0;

        Console.WriteLine("  Running Least-Squares identification...");
        int job = 1;
        Identification.Xident(
            st.W, st.IDAT,
            st.IDA, st.IAD,
            st.LEN, st.NANF, st.IANZ, st.NPOINT,
            st.NYI, st.NU, st.NM,
            dmat, rh, bWork, ipos0, st.IGLW, wksp,
            ref job);

        switch (job)
        {
            case 0:
                st.JOBID = 1;
                Console.WriteLine("  Identification successful.");
                Console.WriteLine();
                Console.WriteLine("  AR recovery (backward Yule-Walker, both poles identifiable):");
                float w1 = st.W[1], w2 = st.W[2];
                float err1 = Math.Abs(w1 - (float)a2);
                float err2 = Math.Abs(w2 - (float)a1);
                Console.WriteLine($"    W[1]: identified={w1,10:F6}   true a2={a2:F6}"
                    + $"   error={err1:F6} ({err1/Math.Abs((float)a2)*100:F2}%)");
                Console.WriteLine($"    W[2]: identified={w2,10:F6}   true a1={a1:F6}"
                    + $"   error={err2:F6} ({err2/(float)a1*100:F2}%)");
                Console.WriteLine($"    W[3] (B_ss[1])={st.W[3]:F6}  W[4] (B_ss[2])={st.W[4]:F6}  (both ≈0 expected)");
                bool ok1 = err1 < 0.05f * (float)Math.Abs(a2);
                bool ok2 = err2 < 0.05f * (float)a1;
                if (ok1 && ok2)
                    Console.WriteLine("    ✓ Both AR coefficients correctly identified (error < 5%)");
                else
                    Console.WriteLine($"   *AR error out of 5% band: W[1]{(ok1 ? "✓" : "✗")}  W[2]{(ok2 ? "✓" : "✗")}");
                Identification.PrintModel(st);
                break;
            case 1:
                Console.WriteLine(" *Singular matrix – unexpected for PRBS excitation.*");
                break;
            case 2:
                Console.WriteLine(" *Error: Unstable auxiliary model.*");
                break;
        }

        // ---- plot both channels ----
        st.IPL[1] = chOut;
        st.IPL[2] = chIn;
        st.NPL    = 2;
        st.JOBPL  = 0;
        ConsoleGraph.PlotChannels(st, len, 0, false);
    }

    // Print identified W[1..N] vs true AR parameters.
    // phi ordering: phi[1]=y[k-N], phi[2]=y[k-N+1], ..., phi[N]=y[k-1]
    // So W[1]=coeff of y[k-N], W[N]=coeff of y[k-1].
    static void CheckStability()
    {
        int n = st.N;
        Console.WriteLine();
        Console.WriteLine("  Identified AR coefficients vs true values:");
        for (int i = 1; i <= n; i++)
            Console.WriteLine(
                $"    W[{i}] coeff(y[k-{n+1-i}]) = {st.W[i],10:G6}  "
              + $"(true: {TrueAR(i, n),10:G6})");
    }

    // True AR coefficients for the PT4 auto-test (all poles at z=e^-0.1≈0.9048).
    // Char poly: (z−a)^4 = z^4 − 4a·z^3 + 6a²·z^2 − 4a³·z + a^4
    // W[1]=coeff(y[k-4])=−a^4, W[2]=4a^3, W[3]=−6a^2, W[4]=4a
    private const double _a = 0.904837418036;
    static float TrueAR(int i, int n) => (n == 4) ? i switch
    {
        1 => (float)(-_a*_a*_a*_a),
        2 => (float)( 4*_a*_a*_a),
        3 => (float)(-6*_a*_a),
        4 => (float)( 4*_a),
        _ => 0f
    } : 0f;

    static void SaveModelToFile(string fn)
    {
        try
        {
            using var sw = new System.IO.StreamWriter(fn);
            var saved = Console.Out;
            Console.SetOut(sw);
            Identification.PrintModel(st);
            Console.SetOut(saved);
            Console.WriteLine($"  Model written to {fn}.");
        }
        catch (Exception ex)
        {
            Console.WriteLine($" *File error: {ex.Message}*");
        }
    }

    // ============================================================
    // Utility input helpers
    // ============================================================

    // Reads the main menu choice: digits 1-9 or the letter 'a'.
    static string ReadMainMenuChoice()
    {
        while (true)
        {
            Console.Write("  Your choice: ");
            string? raw = Console.ReadLine();
            if (raw == null) return "1";  // EOF (e.g. piped input) → exit
            string s = raw.Trim().ToLowerInvariant();
            if (s == "a" || s == "b") return s;
            if (int.TryParse(s, out int v) && v >= 1 && v <= 9) return s;
            Console.WriteLine("  Please enter 1-9, 'a', or 'b'.");
        }
    }

    static int ReadInt(string prompt, int lo, int hi)
    {
        while (true)
        {
            Console.Write(prompt);
            string? s = Console.ReadLine();
            if (int.TryParse(s?.Trim(), out int v) && v >= lo && v <= hi)
                return v;
            Console.WriteLine($"  Please enter a value between {lo} and {hi}.");
        }
    }

    static int ReadIntChoice(string prompt, int[] choices)
    {
        while (true)
        {
            Console.Write(prompt);
            string? s = Console.ReadLine();
            if (int.TryParse(s?.Trim(), out int v) && Array.IndexOf(choices, v) >= 0)
                return v;
            Console.WriteLine($"  Please enter one of: {string.Join(", ", choices)}");
        }
    }

    static float ReadFloat(string prompt, float defaultVal)
    {
        Console.Write($"{prompt} [{defaultVal:G5}]: ");
        string? s = Console.ReadLine();
        if (string.IsNullOrWhiteSpace(s)) return defaultVal;
        return float.TryParse(s.Trim(), out float v) ? v : defaultVal;
    }
}
