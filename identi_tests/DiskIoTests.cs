using Identi;
using Xunit;

namespace IdentTests;

// ============================================================
// Tests for DiskIo – config/data save-load round-trips.
// All tests use a temp file deleted after the test.
// ============================================================
public class DiskIoTests : IDisposable
{
    private readonly string _tmpConfig = Path.GetTempFileName();
    private readonly string _tmpData   = Path.GetTempFileName();

    public void Dispose()
    {
        if (File.Exists(_tmpConfig)) File.Delete(_tmpConfig);
        if (File.Exists(_tmpData))   File.Delete(_tmpData);
    }

    // ----------------------------------------------------------
    // SaveConfig / LoadConfig round-trip
    // ----------------------------------------------------------
    [Fact]
    public void ConfigRoundTrip_BasicFields()
    {
        var st = new AppState();
        st.ITIME  = 42;
        st.ICNEND = 99;
        st.NADU   = 3;
        st.NDAU   = 2;
        st.LEN    = 200;
        st.ITYPMW = 1;
        st.NPL    = 2;
        st.JOBPL  = 1;
        st.FPAR   = 0.5f;

        Assert.True(DiskIo.SaveConfig(_tmpConfig, st));

        var st2 = new AppState();
        Assert.True(DiskIo.LoadConfig(_tmpConfig, st2));

        Assert.Equal(st.ITIME,  st2.ITIME);
        Assert.Equal(st.ICNEND, st2.ICNEND);
        Assert.Equal(st.NADU,   st2.NADU);
        Assert.Equal(st.NDAU,   st2.NDAU);
        Assert.Equal(st.LEN,    st2.LEN);
        Assert.Equal(st.ITYPMW, st2.ITYPMW);
        Assert.Equal(st.NPL,    st2.NPL);
        Assert.Equal(st.JOBPL,  st2.JOBPL);
        Assert.Equal(st.FPAR,   st2.FPAR);
    }

    [Fact]
    public void ConfigRoundTrip_NRDM_NSEQ()
    {
        var st = new AppState();
        st.NRDM[1] = 5;  st.NRDM[2] = 7;
        st.NSEQ[1] = 2;  st.NSEQ[2] = 3;
        st.NRSB[1] = 4;  st.NRSB[2] = 6;

        Assert.True(DiskIo.SaveConfig(_tmpConfig, st));
        var st2 = new AppState();
        Assert.True(DiskIo.LoadConfig(_tmpConfig, st2));

        Assert.Equal(5, st2.NRDM[1]); Assert.Equal(7, st2.NRDM[2]);
        Assert.Equal(2, st2.NSEQ[1]); Assert.Equal(3, st2.NSEQ[2]);
        Assert.Equal(4, st2.NRSB[1]); Assert.Equal(6, st2.NRSB[2]);
    }

    [Fact]
    public void ConfigRoundTrip_ISCALE()
    {
        var st = new AppState();
        for (int i = 1; i <= AppState.NADMA; i++) st.ISCALE[i] = i * 3;

        Assert.True(DiskIo.SaveConfig(_tmpConfig, st));
        var st2 = new AppState();
        Assert.True(DiskIo.LoadConfig(_tmpConfig, st2));

        for (int i = 1; i <= AppState.NADMA; i++)
            Assert.Equal(i * 3, st2.ISCALE[i]);
    }

    [Fact]
    public void ConfigRoundTrip_PMIN_PMAX()
    {
        var st = new AppState();
        for (int i = 1; i <= AppState.NADDAX; i++)
        {
            st.PMIN[i] = -1f * i;
            st.PMAX[i] =  1f * i;
        }

        Assert.True(DiskIo.SaveConfig(_tmpConfig, st));
        var st2 = new AppState();
        Assert.True(DiskIo.LoadConfig(_tmpConfig, st2));

        for (int i = 1; i <= AppState.NADDAX; i++)
        {
            Assert.Equal(-1f * i, st2.PMIN[i]);
            Assert.Equal( 1f * i, st2.PMAX[i]);
        }
    }

    [Fact]
    public void ConfigRoundTrip_TEXT_Labels()
    {
        var st = new AppState();
        st.TEXT[1] = "Chan1";
        st.TEXT[2] = "Chan2";
        st.PBEZ[1] = "Unit1";

        Assert.True(DiskIo.SaveConfig(_tmpConfig, st));
        var st2 = new AppState();
        Assert.True(DiskIo.LoadConfig(_tmpConfig, st2));

        Assert.Equal("Chan1", st2.TEXT[1]);
        Assert.Equal("Chan2", st2.TEXT[2]);
        Assert.Equal("Unit1", st2.PBEZ[1]);
    }

    [Fact]
    public void LoadConfig_WrongFile_ReturnsFalse()
    {
        File.WriteAllBytes(_tmpConfig, new byte[] { 0xDE, 0xAD, 0xBE, 0xEF });
        var st = new AppState();
        Assert.False(DiskIo.LoadConfig(_tmpConfig, st));
    }

    [Fact]
    public void LoadConfig_MissingFile_ReturnsFalse()
    {
        var st = new AppState();
        Assert.False(DiskIo.LoadConfig("nonexistent_xyz_file.dat", st));
    }

    // ----------------------------------------------------------
    // SaveData / LoadData round-trip
    // ----------------------------------------------------------
    [Fact]
    public void DataRoundTrip_SmallBuffer()
    {
        var st = new AppState();
        st.NADU = 1;
        st.NDAU = 1;
        st.LEN  = 10;
        st.NPOINT = 5;

        // fill channels 1 (A/D) and 2 (D/A) with known data
        for (int k = 1; k <= 10; k++)
        {
            st.IDAT[1, k] = (short)(k * 10);
            st.IDAT[2, k] = (short)(k * -5);
        }

        Assert.True(DiskIo.SaveData(_tmpData, st));

        var st2 = new AppState();
        Assert.True(DiskIo.LoadData(_tmpData, st2));

        Assert.Equal(st.NPOINT, st2.NPOINT);
        for (int k = 1; k <= 10; k++)
        {
            Assert.Equal((short)(k * 10), st2.IDAT[1, k]);
            Assert.Equal((short)(k * -5), st2.IDAT[2, k]);
        }
    }

    [Fact]
    public void DataRoundTrip_LEN_Preserved()
    {
        var st = new AppState();
        st.NADU = 2; st.NDAU = 0;
        st.LEN  = 50;
        for (int k = 1; k <= 50; k++)
            for (int ch = 1; ch <= 2; ch++)
                st.IDAT[ch, k] = (short)(k + ch * 100);

        DiskIo.SaveData(_tmpData, st);

        var st2 = new AppState();
        DiskIo.LoadData(_tmpData, st2);

        // The LEN read from file overrides st2.LEN via ReadKonfig
        for (int k = 1; k <= 50; k++)
            for (int ch = 1; ch <= 2; ch++)
                Assert.Equal(st.IDAT[ch, k], st2.IDAT[ch, k]);
    }

    [Fact]
    public void LoadData_MissingFile_ReturnsFalse()
    {
        var st = new AppState();
        Assert.False(DiskIo.LoadData("missing_data_file.bin", st));
    }

    [Fact]
    public void LoadData_WrongMagic_ReturnsFalse()
    {
        // Write a config file (wrong magic for data reader)
        var st = new AppState();
        st.NADU = 1; st.NDAU = 0; st.LEN = 5;
        DiskIo.SaveConfig(_tmpConfig, st);

        var st2 = new AppState();
        Assert.False(DiskIo.LoadData(_tmpConfig, st2));
    }

    [Fact]
    public void LoadConfig_WrongMagicForDataFile_ReturnsFalse()
    {
        var st = new AppState();
        st.NADU = 1; st.NDAU = 0; st.LEN = 5;
        DiskIo.SaveData(_tmpData, st);

        var st2 = new AppState();
        Assert.False(DiskIo.LoadConfig(_tmpData, st2));
    }
}
