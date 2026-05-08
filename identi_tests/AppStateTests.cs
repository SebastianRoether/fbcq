using Identi;
using Xunit;

namespace IdentTests;

// ============================================================
// Tests for AppState – constructor defaults (BLOCK DATA values).
// ============================================================
public class AppStateTests
{
    [Fact]
    public void Constructor_ArraySizes_Correct()
    {
        var st = new AppState();
        // IDAT must hold [NADDAX+1, LENMA+1]
        Assert.Equal(AppState.NADDAX + 1, st.IDAT.GetLength(0));
        Assert.Equal(AppState.LENMA  + 1, st.IDAT.GetLength(1));
    }

    [Fact]
    public void Constructor_NRDM_DefaultValues()
    {
        var st = new AppState();
        Assert.Equal(11, st.NRDM[1]);
        Assert.Equal(11, st.NRDM[2]);
    }

    [Fact]
    public void Constructor_NSEQ_DefaultValues()
    {
        var st = new AppState();
        Assert.Equal(1, st.NSEQ[1]);
        Assert.Equal(1, st.NSEQ[2]);
    }

    [Fact]
    public void Constructor_NRSB_DefaultValues()
    {
        var st = new AppState();
        Assert.Equal(7, st.NRSB[1]);
        Assert.Equal(7, st.NRSB[2]);
    }

    [Fact]
    public void Constructor_NPL_IsOne()
    {
        var st = new AppState();
        Assert.Equal(1, st.NPL);
    }

    [Fact]
    public void Constructor_IPL1_IsOne()
    {
        var st = new AppState();
        Assert.Equal(1, st.IPL[1]);
    }

    [Fact]
    public void Constructor_ISCALE_AllOne()
    {
        var st = new AppState();
        for (int i = 1; i <= AppState.NADMA; i++)
            Assert.Equal(1, st.ISCALE[i]);
    }

    [Fact]
    public void Constructor_ADCLimits_Correct()
    {
        var st = new AppState();
        Assert.Equal(0,    st.IADMIN);
        Assert.Equal(4096, st.IADMAX);
        Assert.Equal(0,    st.IDAMIN);
        Assert.Equal(2048, st.IDAMAX);
    }

    [Fact]
    public void Constructor_W_AllZero()
    {
        var st = new AppState();
        for (int i = 1; i <= AppState.NPWMA; i++)
            Assert.Equal(0f, st.W[i]);
    }

    [Fact]
    public void Constructor_Constants_Correct()
    {
        Assert.Equal(2,    AppState.NDAMA);
        Assert.Equal(8,    AppState.NADMA);
        Assert.Equal(10,   AppState.NADDAX);
        Assert.Equal(1000, AppState.LENMA);
        Assert.Equal(400,  AppState.NPWMA);
    }

    [Fact]
    public void Constructor_PMIN_PMAX_AllZero()
    {
        var st = new AppState();
        for (int i = 1; i <= AppState.NADDAX; i++)
        {
            Assert.Equal(0f, st.PMIN[i]);
            Assert.Equal(0f, st.PMAX[i]);
        }
    }
}
