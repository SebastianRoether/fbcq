namespace Identi;

// ============================================================
// DiskIo – save and restore configuration and measurement data.
//
// Replaces the unformatted binary Fortran DREAD/DWRITE routines.
// Uses a simple binary format: a header magic word followed by
// the raw COMMON block fields.
//
// Also handles the Honeywell H-TMS-3000 CSV import
// (FORTRAN job 4500 / job 5 in the disk sub-menu).
// ============================================================

internal static class DiskIo
{
    private const uint MAGIC_CFG  = 0x49444346u; // "IDCF"
    private const uint MAGIC_DATA = 0x49444441u; // "IDDA"

    // ----------------------------------------------------------
    // SaveConfig – write KONFIG + PTEXTE to file.
    // (Fortran JOB=3 in the disk sub-menu)
    // ----------------------------------------------------------
    public static bool SaveConfig(string fileName, AppState st)
    {
        try
        {
            using var bw = new BinaryWriter(File.Open(fileName, FileMode.Create));
            bw.Write(MAGIC_CFG);
            WriteKonfig(bw, st);
            WritePtexte(bw, st);
            return true;
        }
        catch (Exception ex)
        {
            Console.WriteLine($" *File error: {ex.Message}*");
            return false;
        }
    }

    // ----------------------------------------------------------
    // LoadConfig – read KONFIG + PTEXTE from file.
    // (Fortran JOB=1 in the disk sub-menu)
    // ----------------------------------------------------------
    public static bool LoadConfig(string fileName, AppState st)
    {
        try
        {
            using var br = new BinaryReader(File.OpenRead(fileName));
            uint magic = br.ReadUInt32();
            if (magic != MAGIC_CFG) throw new InvalidDataException("Not a config file.");
            ReadKonfig(br, st);
            ReadPtexte(br, st);
            st.IONLFL = 0; // online flag reset after load
            return true;
        }
        catch (Exception ex)
        {
            Console.WriteLine($" *File error: {ex.Message}*");
            return false;
        }
    }

    // ----------------------------------------------------------
    // SaveData – write KONFIG + PTEXTE + ROHDAT (ring buffer) to file.
    // (Fortran JOB=4)
    // ----------------------------------------------------------
    public static bool SaveData(string fileName, AppState st)
    {
        try
        {
            using var bw = new BinaryWriter(File.Open(fileName, FileMode.Create));
            bw.Write(MAGIC_DATA);
            WriteKonfig(bw, st);
            WritePtexte(bw, st);
            bw.Write(st.LEN);
            bw.Write(st.NPOINT);
            int nadda = st.NADU + st.NDAU;
            for (int j = 1; j <= st.LEN; j++)
                for (int i = 1; i <= nadda; i++)
                    bw.Write(st.IDAT[i, j]);
            return true;
        }
        catch (Exception ex)
        {
            Console.WriteLine($" *File error: {ex.Message}*");
            return false;
        }
    }

    // ----------------------------------------------------------
    // LoadData – read KONFIG + PTEXTE + ROHDAT from file.
    // (Fortran JOB=2)
    // ----------------------------------------------------------
    public static bool LoadData(string fileName, AppState st)
    {
        try
        {
            using var br = new BinaryReader(File.OpenRead(fileName));
            uint magic = br.ReadUInt32();
            if (magic != MAGIC_DATA) throw new InvalidDataException("Not a data file.");
            ReadKonfig(br, st);
            ReadPtexte(br, st);
            int len    = br.ReadInt32();
            st.NPOINT  = br.ReadInt32();
            int nadda  = st.NADU + st.NDAU;
            for (int j = 1; j <= len; j++)
                for (int i = 1; i <= nadda; i++)
                    st.IDAT[i, j] = br.ReadInt16();
            return true;
        }
        catch (Exception ex)
        {
            Console.WriteLine($" *File error: {ex.Message}*");
            return false;
        }
    }

    // ----------------------------------------------------------
    // ImportHtms – import Honeywell H-TMS-3000 ASCII file.
    // Format: index  value1  value2  (two columns of floating point)
    // Fortran JOB=5 in disk sub-menu.
    // ----------------------------------------------------------
    public static bool ImportHtms(string fileName, AppState st,
                                   int targetChannel,
                                   float physMin, float physMax,
                                   int scaleFactor, int shiftRows, int decimate)
    {
        try
        {
            var lines = File.ReadAllLines(fileName);
            if (lines.Length < 3)
                throw new InvalidDataException("H-TMS file too short.");

            // skip two header lines
            int lineIdx = 2 + shiftRows;
            float rdif = physMax - physMin;
            if (targetChannel <= st.NADMAX)
            {
                if (scaleFactor > 0) rdif *= scaleFactor;
                else if (scaleFactor < 0) rdif /= Math.Abs(scaleFactor);
            }
            int adRange = (targetChannel <= st.NADMAX)
                          ? (st.IADMAX - st.IADMIN)
                          : (st.IDAMAX - st.IDAMIN);

            for (int i = 1; i <= st.LEN; i++)
            {
                float sum = 0f;
                int count = 0;
                for (int k = 0; k < Math.Max(1, decimate); k++, lineIdx++)
                {
                    if (lineIdx >= lines.Length) { st.LEN = i - 1; goto done; }
                    var parts = lines[lineIdx].Split(
                        new char[]{' ','\t'}, StringSplitOptions.RemoveEmptyEntries);
                    if (parts.Length >= 3 && float.TryParse(parts[2], out float v2))
                    {
                        sum += v2;
                        count++;
                    }
                }
                if (count == 0) { st.LEN = i - 1; break; }
                float avg = sum / count;
                st.IDAT[targetChannel, i] = (short)Math.Round(
                    ((avg - physMin) / rdif) * adRange);
            }
            done:
            return true;
        }
        catch (Exception ex)
        {
            Console.WriteLine($" *File error: {ex.Message}*");
            return false;
        }
    }

    // ---- private helpers ----

    private static void WriteKonfig(BinaryWriter bw, AppState st)
    {
        bw.Write(st.ITIME);  bw.Write(st.ICNEND);
        bw.Write(st.NADU);   bw.Write(st.NDAU);
        bw.Write(st.LEN);    bw.Write(st.ITYPMW);
        for (int i = 1; i <= AppState.NADDAX; i++) bw.Write(st.IMW  [i]);
        for (int i = 1; i <= AppState.NDAMA;  i++) bw.Write(st.IVMA [i]);
        for (int i = 1; i <= AppState.NDAMA;  i++) bw.Write(st.NRDM [i]);
        for (int i = 1; i <= AppState.NDAMA;  i++) bw.Write(st.NSEQ [i]);
        for (int i = 1; i <= AppState.NDAMA;  i++) bw.Write(st.NRSB [i]);
        for (int i = 1; i <= AppState.NADDAX; i++) bw.Write(st.IPL  [i]);
        bw.Write(st.NPL); bw.Write(st.JOBPL); bw.Write(st.FPAR);
        for (int i = 1; i <= AppState.NADMA;  i++) bw.Write(st.ISCALE[i]);
        for (int i = 1; i <= AppState.NADDAX; i++) bw.Write(st.PMIN [i]);
        for (int i = 1; i <= AppState.NADDAX; i++) bw.Write(st.PMAX [i]);
    }

    private static void ReadKonfig(BinaryReader br, AppState st)
    {
        st.ITIME  = br.ReadInt32();  st.ICNEND = br.ReadInt32();
        st.NADU   = br.ReadInt32();  st.NDAU   = br.ReadInt32();
        st.LEN    = br.ReadInt32();  st.ITYPMW = br.ReadInt32();
        for (int i = 1; i <= AppState.NADDAX; i++) st.IMW  [i] = br.ReadInt32();
        for (int i = 1; i <= AppState.NDAMA;  i++) st.IVMA [i] = br.ReadInt32();
        for (int i = 1; i <= AppState.NDAMA;  i++) st.NRDM [i] = br.ReadInt32();
        for (int i = 1; i <= AppState.NDAMA;  i++) st.NSEQ [i] = br.ReadInt32();
        for (int i = 1; i <= AppState.NDAMA;  i++) st.NRSB [i] = br.ReadInt32();
        for (int i = 1; i <= AppState.NADDAX; i++) st.IPL  [i] = br.ReadInt32();
        st.NPL    = br.ReadInt32();
        st.JOBPL  = br.ReadInt32();
        st.FPAR   = br.ReadSingle();
        for (int i = 1; i <= AppState.NADMA;  i++) st.ISCALE[i] = br.ReadInt32();
        for (int i = 1; i <= AppState.NADDAX; i++) st.PMIN [i] = br.ReadSingle();
        for (int i = 1; i <= AppState.NADDAX; i++) st.PMAX [i] = br.ReadSingle();
    }

    private static void WritePtexte(BinaryWriter bw, AppState st)
    {
        for (int i = 1; i <= AppState.NADDAX; i++) bw.Write(st.TEXT[i] ?? "");
        for (int i = 1; i <= AppState.NADDAX; i++) bw.Write(st.PBEZ[i] ?? "");
    }

    private static void ReadPtexte(BinaryReader br, AppState st)
    {
        for (int i = 1; i <= AppState.NADDAX; i++) st.TEXT[i] = br.ReadString();
        for (int i = 1; i <= AppState.NADDAX; i++) st.PBEZ[i] = br.ReadString();
    }
}
