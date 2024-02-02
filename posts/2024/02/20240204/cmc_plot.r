source("lib/library.r")


# PREAMBLE


# Ratios:
# - Debt-to-Equity Ratio
#   - Debt / (Total Assets - Total Liabilities)
#   - This measures the proportion of debt financing compared to equity
#     financing.
# - Debt-to-Total Assets Ratio
#   - Debt / Total Assets
#   - This measures the proportion of debt financing compared to all assets.
# - Current Ratio
#   - Current Assets / Current Liabilities
#   - This measures a company's ability to meet short-term obligations with its
#     short-term assets.
# - Cash Ratio
#   - Cash / Current Liabilities
#   - This measures a company's ability to meet short-term obligations with its
#     most liquid asset.
# - Asset Turnover Ratio
#   - Revenue / Total Assets
#   - This measures how efficiently a company uses its assets to generate
#     revenue.
# - Return on Assets (ROA)
#   - Income / Total Assets
#   - This measures the profitability of a company relative to its total
#     assets.

# Derived Line Items:
# - Net Income
#   - Revenue - Expenses
# - Total Liabilities + Equity
#   - Total Assets


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
