source("lib/library.r")


# SETUP


df <- here("cmc_full.csv") %>%
    read_csv()


# P/S ANALYSIS


df_ps <- df %>%
    select(ticker, year, cap, rev) %>%
    mutate(ps = cap / rev,
           lg_ps = log(ps)) %>%
    na.omit()

df_yr_cap <- df_ps %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_ps %>%
    group_by(year) %>%
    summarize(lg_ps_cap = mean(cap * lg_ps)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_ps = lg_ps_cap / cap,
           ps = exp(lg_ps))

df_yr %>%
    with({
        pdf(here("ps_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, ps, type = "n",
             main = "Cap-Weighted P/S of Current 100 Largest US Companies",
             xlab = "", ylab = "Price-to-Sales",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = 1:6, v = 2000:2030, col = "lightblue")
        lines(year, ps, lwd = 2)

        par(op)
        dev.off()
    })


# P/E ANALYSIS


df_pe <- df %>%
    select(ticker, year, cap, inc) %>%
    mutate(pe = cap / inc,
           lg_pe = log(pe)) %>%
    na.omit()

df_yr_cap <- df_pe %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_pe %>%
    group_by(year) %>%
    summarize(lg_pe_cap = mean(cap * lg_pe)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_pe = lg_pe_cap / cap,
           pe = exp(lg_pe))

df_yr %>%
    with({
        pdf(here("pe_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, pe, type = "n",
             main = "Cap-Weighted P/E of Current 100 Largest US Companies",
             xlab = "", ylab = "Price-to-Income",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, pe, lwd = 2)

        par(op)
        dev.off()
    })


# P/B ANALYSIS


df_pb <- df %>%
    select(ticker, year, cap, ast, lia) %>%
    mutate(book = ast - lia,
           pb = cap / book,
           lg_pb = log(pb)) %>%
    na.omit() %>%
    filter(is.finite(lg_pb))

df_yr_cap <- df_pb %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_pb %>%
    group_by(year) %>%
    summarize(lg_pb_cap = mean(cap * lg_pb)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_pb = lg_pb_cap / cap,
           pb = exp(lg_pb))

df_yr %>%
    with({
        pdf(here("pb_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, pb, type = "n",
             main = "Cap-Weighted P/B of Current 100 Largest US Companies",
             xlab = "", ylab = "Price-to-Book",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, pb, lwd = 2)

        par(op)
        dev.off()
    })


# P/C ANALYSIS


df_pc <- df %>%
    select(ticker, year, cap, csh) %>%
    mutate(pc = cap / csh,
           lg_pc = log(pc)) %>%
    na.omit() %>%
    filter(is.finite(lg_pc))

df_yr_cap <- df_pc %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_pc %>%
    group_by(year) %>%
    summarize(lg_pc_cap = mean(cap * lg_pc)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_pc = lg_pc_cap / cap,
           pc = exp(lg_pc))

df_yr %>%
    with({
        pdf(here("pc_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, pc, type = "n",
             main = "Cap-Weighted P/C of Current 100 Largest US Companies",
             xlab = "", ylab = "Price-to-Cash",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, pc, lwd = 2)

        par(op)
        dev.off()
    })


# D/A ANALYSIS


df_da <- df %>%
    select(ticker, year, cap, dbt, ast) %>%
    mutate(da = dbt / ast,
           lg_da = log(da)) %>%
    na.omit() %>%
    filter(is.finite(lg_da))

df_yr_cap <- df_da %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_da %>%
    group_by(year) %>%
    summarize(lg_da_cap = mean(cap * lg_da)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_da = lg_da_cap / cap,
           da = exp(lg_da))

df_yr %>%
    with({
        pdf(here("da_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, da, type = "n",
             main = "Cap-Weighted D/A of Current 100 Largest US Companies",
             xlab = "", ylab = "Debt-to-Assets",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, da, lwd = 2)

        par(op)
        dev.off()
    })


# D/B ANALYSIS


df_db <- df %>%
    select(ticker, year, cap, dbt, ast, lia) %>%
    mutate(book = ast - lia,
           db = dbt / book,
           lg_db = log(db)) %>%
    na.omit() %>%
    filter(is.finite(lg_db))

df_yr_cap <- df_db %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_db %>%
    group_by(year) %>%
    summarize(lg_db_cap = mean(cap * lg_db)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_db = lg_db_cap / cap,
           db = exp(lg_db))

df_yr %>%
    with({
        pdf(here("db_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, db, type = "n",
             main = "Cap-Weighted D/B of Current 100 Largest US Companies",
             xlab = "", ylab = "Debt-to-Book",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, db, lwd = 2)

        par(op)
        dev.off()
    })


# MARGIN ANALYSIS


df_mar <- df %>%
    select(ticker, year, cap, rev, inc) %>%
    mutate(mar = inc / rev,
           lg_mar = log(mar)) %>%
    na.omit() %>%
    filter(is.finite(lg_mar))

df_yr_cap <- df_mar %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_mar %>%
    group_by(year) %>%
    summarize(lg_mar_cap = mean(cap * lg_mar)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_mar = lg_mar_cap / cap,
           mar = exp(lg_mar))

df_yr %>%
    with({
        pdf(here("margin.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, mar, type = "n",
             main = "Cap-Weighted Profit Margin of Current 100 Largest US Companies",
             xlab = "", ylab = "Income-to-Revenue",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, mar, lwd = 2)

        par(op)
        dev.off()
    })


# ROA ANALYSIS


df_roa <- df %>%
    select(ticker, year, cap, inc, ast) %>%
    mutate(roa = inc / ast,
           lg_roa = log(roa)) %>%
    na.omit() %>%
    filter(is.finite(lg_roa))

df_yr_cap <- df_roa %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_roa %>%
    group_by(year) %>%
    summarize(lg_roa_cap = mean(cap * lg_roa)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_roa = lg_roa_cap / cap,
           roa = exp(lg_roa))

df_yr %>%
    with({
        pdf(here("roa.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, roa, type = "n",
             main = "Cap-Weighted ROA of Current 100 Largest US Companies",
             xlab = "", ylab = "Income-to-Assets",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, roa, lwd = 2)

        par(op)
        dev.off()
    })


# ROE ANALYSIS


df_roe <- df %>%
    select(ticker, year, cap, inc, ast, lia) %>%
    mutate(eqt = ast - lia,
           roe = inc / eqt,
           lg_roe = log(roe)) %>%
    na.omit() %>%
    filter(is.finite(lg_roe))

df_yr_cap <- df_roe %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_roe %>%
    group_by(year) %>%
    summarize(lg_roe_cap = mean(cap * lg_roe)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_roe = lg_roe_cap / cap,
           roe = exp(lg_roe))

df_yr %>%
    with({
        pdf(here("roe.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, roe, type = "n",
             main = "Cap-Weighted ROE of Current 100 Largest US Companies",
             xlab = "", ylab = "Income-to-Equity",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 100, 2.5), v = 2000:2030, col = "lightblue")
        lines(year, roe, lwd = 2)

        par(op)
        dev.off()
    })


# P/EV ANALYSIS


df_pev <- df %>%
    select(ticker, year, cap, dbt, csh) %>%
    mutate(ev = cap + dbt - csh,
           pev = cap / ev,
           lg_pev = log(pev)) %>%
    na.omit() %>%
    filter(is.finite(lg_pev))

df_yr_cap <- df_pev %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_pev %>%
    group_by(year) %>%
    summarize(lg_pev_cap = mean(cap * lg_pev)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_pev = lg_pev_cap / cap,
           pev = exp(lg_pev))

df_yr %>%
    with({
        pdf(here("pev_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, pev, type = "n",
             main = "Cap-Weighted P/EV of Current 100 Largest US Companies",
             xlab = "", ylab = "Price-to-Enterprise Value",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 1, 0.05), v = 2000:2030, col = "lightblue")
        lines(year, pev, lwd = 2)

        par(op)
        dev.off()
    })


# E/D ANALYSIS


df_ed <- df %>%
    select(ticker, year, cap, inc, dbt) %>%
    mutate(ed = inc / dbt,
           lg_ed = log(ed)) %>%
    na.omit() %>%
    filter(is.finite(lg_ed))

df_yr_cap <- df_ed %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_ed %>%
    group_by(year) %>%
    summarize(lg_ed_cap = mean(cap * lg_ed)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_ed = lg_ed_cap / cap,
           ed = exp(lg_ed))

df_yr %>%
    with({
        pdf(here("ed_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, ed, type = "n",
             main = "Cap-Weighted E/D of Current 100 Largest US Companies",
             xlab = "", ylab = "Income-to-Debt",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 1, 0.05), v = 2000:2030, col = "lightblue")
        lines(year, ed, lwd = 2)

        par(op)
        dev.off()
    })


# S/D ANALYSIS


df_sd <- df %>%
    select(ticker, year, cap, rev, dbt) %>%
    mutate(sd = rev / dbt,
           lg_sd = log(sd)) %>%
    na.omit() %>%
    filter(is.finite(lg_sd))

df_yr_cap <- df_sd %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_sd %>%
    group_by(year) %>%
    summarize(lg_sd_cap = mean(cap * lg_sd)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_sd = lg_sd_cap / cap,
           sd = exp(lg_sd))

df_yr %>%
    with({
        pdf(here("sd_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, sd, type = "n",
             main = "Cap-Weighted S/D of Current 100 Largest US Companies",
             xlab = "", ylab = "Revenue-to-Debt",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 1, 0.05), v = 2000:2030, col = "lightblue")
        lines(year, sd, lwd = 2)

        par(op)
        dev.off()
    })


# ULTIMATE ANALYSIS


df_ult <- df %>%
    mutate(ult = (log(cap / rev) + log(cap / inc) + log(cap / inc) + log(cap / csh) + log(lia / rev) + log(lia / inc) + log(lia / inc) + log(lia / csh) + log(dbt / rev) + log(dbt / inc) + log(dbt / inc) + log(dbt / csh)) / 12,
           lg_ult = ult) %>%
    na.omit() %>%
    filter(is.finite(lg_ult))

df_yr_cap <- df_ult %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_ult %>%
    group_by(year) %>%
    summarize(lg_ult_cap = mean(cap * lg_ult)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_ult = lg_ult_cap / cap,
           ult = exp(lg_ult))

df_yr %>%
    with({
        pdf(here("ult_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, ult, type = "n",
             main = "Cap-Weighted ??? of Current 100 Largest US Companies",
             xlab = "", ylab = "???",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = seq(0, 1, 0.05), v = 2000:2030, col = "lightblue")
        lines(year, ult, lwd = 2)

        par(op)
        dev.off()
    })
