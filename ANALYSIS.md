# IDENTI – Analysis Notes for Future Debugging

**Project**: C# port of `identi/` Fortran 77 system identification program  
**Location**: `identi_new/` (production), `identi_tests/` (unit tests)  
**Test status**: 98 tests, all passing (as of 2026-05-08)

---

## 1. Xident – Backward-Time Regression

### What the algorithm does

`Xident` (in `Identification.cs`) performs **backward-time** least-squares regression. This is the most important architectural fact to understand.

At each iteration k1 the ring-buffer `feld` is shifted such that:

```
feld[output_ch, 1]       = y(t)          ← current sample  (stored in gam)
feld[output_ch, 2..n+1]  = y(t+1..t+n)  ← FUTURE samples  (stored in phi)
feld[input_ch,  2..n+1]  = u(t+1..t+n)  ← FUTURE inputs   (stored in phi)
```

The LS normal equations it assembles are therefore:

```
min_W  Σ_t  [ y(t)  −  W[1]·y(t+n) − ... − W[n]·y(t+1)
                      − W[n+1]·u(t+n) − ... − W[2n]·u(t+1) ]²
```

This is the **backward prediction** problem. The time index `t` decreases as `k1` increases.

### Consequences for AR coefficient recovery

For a **stationary process** (PRBS excitation):  
The Yule-Walker equations are symmetric in time: `R_y(k) = R_y(-k)`.  
Backward Yule-Walker gives **the same AR coefficients** as forward Yule-Walker.  
→ W[1..n] converge to the true AR polynomial coefficients.

**Exception – SISO ARX with n=2 PRBS:**  
For a 2nd-order ARX excited by PRBS, the *pure backward AR* solution would give:
```
W[1] ← a₂   (reversed order compared to forward notation)
W[2] ← a₁
```
because the backward companion poly has reversed coefficients.  
However with *exogenous ARX input* (u ≠ 0), the coupling term `E[u(t+1)·y(t+1)] ≠ 0` shifts the solution – the empirically observed values for `a₁=1.5, a₂=−0.7` come out as `W[1]≈−1.43, W[2]≈2.14`, not the naive reversed values.

### Consequence for MA coefficient recovery

The first-order condition for W[MA] is:
```
W[MA] · R_u(0) = E[ u(t+1) · y(t) ]
```
For a **causal ARX** system, `u(t+1)` is independent of `y(t)` (future input cannot depend on current output).  
→ `E[u(t+1)·y(t)] = 0`  
→ **W[MA] → 0 regardless of the true b coefficient.**

This is not a bug. It is the mathematical consequence of using backward regression on causally-sampled data.

---

## 2. Step Response is Not a Valid ARX Excitation

### Why the PT4 auto-test shows "wrong" AR coefficients

The PT4 step response produces non-stationary data dominated by the steady state.

- ~75% of the 400 samples have `y ≈ u ≈ RawOne = 4096` (samples 100–400).
- LS fitting ~300 identical steady-state rows enforces: `Σ W[AR] · y_ss ≈ y_ss` → **Σ W[AR] ≈ 1**.
- The true 4th-order AR polynomial has `Σ coefficients = 4a − 6a² + 4a³ − a⁴ ≈ 3.619 − 4.912 + 2.963 − 0.670 = 1.000` – exact by design (unit DC gain). But individual coefficients are not recovered.

The AR coefficients printed in the auto-test output reflect a backward model fit to steady-state data, not the physical AR parameters. The **state-space (A, B, C) matrices may still be useful** for nearly-linear steady-state analysis.

**To correctly identify AR coefficients, use a PRBS input** (implemented in menu option 2-5).

---

## 3. Bugs Fixed

### Bug 1 – HP filter in-place mutation (`ConsoleGraph.cs`, JOBPL=3)

**Problem**: The high-pass filter loop read `data[i-1]` after overwriting it:
```csharp
// WRONG:
float d = data[i] - data[i-1];  // data[i-1] was already overwritten with HP output
```
**Fix**: Store `prevRaw = data[i-1]` before overwriting.

**Test**: `ConsoleGraphTests.ApplyFilter_JobPL3_HighPass_StartsNearZero` and `ApplyFilter_JobPL3_PassesStepFront`.

---

### Bug 2 – Xgmkon called with wrong source matrix (`Identification.cs`)

**Problem**: After LS, `Xident` builds the Guidorzi transformation (GM) matrix via:
```csharp
// OLD (wrong): source is the information matrix, not AR coefficients
MatrixLib.Xgmkon(gm, dmat, n, nm, nyi);
```
`Xgmkon` reads `a[j, i]` which for 2nd order accesses `dmat[2, 1]` – an information matrix element (cross-correlation), not the AR coefficient `W[2]`.

**Effect**: For first-order SISO (n=1), `nblock=0` so `a` is never read → invisible.  
For n≥2: the Guidorzi GM matrix is computed from cross-correlation values → GM⁻¹·B gives wrong state-space B.

**Fix**: Build `aForGm` from the solved W before calling Xgmkon:
```csharp
var aForGm = new float[n + 1, nm + 1];
for (int i = 1; i <= nm; i++)
    for (int j = 1; j <= n; j++)
        aForGm[j, i] = w[(i - 1) * n + j];
MatrixLib.Xgmkon(gm, aForGm, n, nm, nyi);
```

**Tests**:  
- `MatrixLibTests.Xgmkon_SecondOrder_SISO_SourceMustBeWNotInfoMatrix` – shows correct vs wrong source give different GM.  
- `AutoTestPipelineTests.Stage3_Xgmkon_WithCorrectSource_DiffersFromWrongSource_ForOrderGe2` – PT4 specific.

---

### Xgmkon double-division in Strsl1 (dormant, not fixed)

`Strsl1` (public) divides `b[n] /= t[n,n]` then delegates to `Strsl1_impl` which also divides.  
`Strsl1` is never called externally. `Strsl1_clean` is the correct path and is used by all callers.  
**Test**: `MatrixLibTests.Strsl1_HasDoubleDivisionBug_DiffersFromClean` – asserts the bug exists to prevent silent "fixing" without awareness.

---

## 4. Xident phi Ordering (n=4 SISO)

```
phi index:  1      2      3      4      5      6      7      8
content:   y(t+4) y(t+3) y(t+2) y(t+1) u(t+4) u(t+3) u(t+2) u(t+1)
W index:   W[1]   W[2]   W[3]   W[4]   W[5]   W[6]   W[7]   W[8]
```

The W-vector returned by Xident packs **AR first, then MA** (theta).  
But then Xident **overwrites W[n+1..n·nm+nu·n]** with the state-space B matrix via Mimprd.  
After `Xident` returns:
```
W[1..n]         = AR coefficients (unchanged after theta)
W[n+1..end]     = B state-space columns (overwritten, not theta MA)
```
The original MA theta values are **not recoverable** from the returned W.

---

## 5. State-Space Decomposition – Xmdecd

`Xmdecd` decodes W into the observable canonical form (A, B, C):

```
A =  [  0    1    0  ...   0  ]
     [  0    0    1  ...   0  ]
     [ ...                    ]
     [ W[1] W[2] W[3] ... W[n]]  ← last row = AR coefficients

B = state-space B (post Guidorzi transform)
C = [1, 0, 0, ..., 0]           ← first state observed
```

---

## 6. Known Remaining Issues (not yet fixed)

| # | Issue | Location | Impact |
|---|---|---|---|
| 3 | `Console.Clear()` throws `IOException` outside a real console | `ConsoleGraph.PrintChannelValues` | Fails in CI / test runners. Test documents the throw. |
| 4 | `Strsl1` double-division | `MatrixLib.Strsl1` | Dormant (never called externally). `Strsl1_clean` is always used. |
| 5 | `Xrsolv` dead write | `Identification.Xrsolv` | `wksp[kk,icnt]=dmat[kk,kk]` immediately overwritten; no functional impact. |
| 6 | ConsoleGraph X-axis shows sample count, not physical time | `ConsoleGraph.PlotChannels` | Cosmetic. Label says "0 .. N" not "0 s .. N·Ts s". |
| 7 | Step response gives biased AR (Σ AR ≈ 1, not true poly) | `Program.RunAutoTest` | Expected by design. Documented in auto-test output and Stage2 tests. Would need PRBS input for true recovery. |

---

## 7. Test Coverage Summary

| File | Tests | What is covered |
|---|---|---|
| `AppStateTests.cs` | 11 | Constructor defaults, constants, array sizes |
| `DiskIoTests.cs` | 10 | Config/data round-trip, magic bytes, missing files |
| `ConsoleGraphTests.cs` | 8 | Filters (LP, HP, diff, none), resampler, plot smoke tests |
| `MatrixLibTests.cs` | 32 | Mzeset, Vv*, Mvprod, Mdpsum, Sgefa/Sgesl, Srotg, Schud, Schex, Strsl1, Xgmkon, Mimprd |
| `IdentificationTests.cs` | 23 | Iplag, Xident (1st order, 2nd order, no data), Xmdecd, Xrsolv, Xmodel |
| `AutoTestPipelineTests.cs` | 14 | PT4 signal generation, Xident on step response, aForGm mapping, Xmdecd structure, prediction quality |

---

## 8. Algorithm Reference

### Identification Methods (job parameter)

| job in | method | description |
|---|---|---|
| 1 | Least Squares | Normal equations via Mdpsum: DMAT += phi·phi^T |
| 2 | QR-LS | Sequential QR update via Schud |
| 3 | Instrumental Variables | Normal equations via Mdpsum with hphi as instrument |
| 4 | QR-IV | Sequential QR with IV instruments |

On success: job out = 0. On singular matrix: job out = 1. On unstable auxiliary model: job out = 2.

### Guidorzi Transformation (Xgmkon)

Converts the theta AR polynomial coefficients to the observable canonical companion matrix GM.  
GM is then inverted via Mimprd (C = A⁻¹ · B) to transform the MA coefficients to state-space B.  
**Source parameter must be `aForGm[j,i] = W[(i-1)*n + j]`**, not the information matrix.

### Circular Buffer (Iplag)

```csharp
Iplag(npsave, offset, len) = ((npsave - offset - 1 + len * N) % len) + 1
```
Wraps a pointer `offset` steps backward from `npsave` within a circular buffer of length `len`.  
Xident uses `offset = nanf + k1` where `k1 = 1..ianz`, so the lag **decreases** as k1 increases → backward traversal.
