source("lib/library.r")


# SETUP


df <- here("cmc_full.csv") %>%
    read_csv()


# FILTERING


df_mini <- df %>%
    filter(year < 2024) %>%
    select(ticker, year, cap, inc) %>%
    group_by(ticker) %>%
    mutate(len = length(ticker),
           na = sum(is.na(c(cap, inc)))) %>%
    filter(na == 0) %>%
    ungroup()

df_filt <- df %>%
    filter(year < 2023) %>%
    group_by(ticker) %>%
    mutate(na = sum(is.na(c(cap, rev, inc, ast, lia, dbt, csh))),
           len = length(cap)) %>%
    ungroup() %>%
    filter(na == 0, len == max(len))

df_filt %>%
    pull(ticker) %>%
    unique() %>%
    sort() %>%
    cat(., "\n", sep = ", ")

df_mini %>%
    pull(ticker) %>%
    unique() %>%
    length()


# TRANSFORMATION


df_yr <- df_filt %>%
    select(-ticker) %>%
    group_by(year) %>%
    summarize_all(mean) %>%
    arrange(-year)

df_yr <- df_mini %>%
    select(-ticker, -len, -na) %>%
    group_by(year) %>%
    summarize(cap_w = weighted.mean(cap, cap),
              val_w = weighted.mean(inc, cap),
              val = mean(inc),
              cap = mean(cap)) %>%
    arrange(-year)


# CAP WEIGHTED


df_yr <- df_filt %>%
    select(-ticker) %>%
    group_by(year) %>%
    summarize(rev = weighted.mean(rev, cap),
              inc = weighted.mean(inc, cap),
              ast = weighted.mean(ast, cap),
              lia = weighted.mean(lia, cap),
              dbt = weighted.mean(dbt, cap),
              csh = weighted.mean(csh, cap),
              cap = weighted.mean(cap, cap)) %>%
    arrange(-year)


# PLOTTING


df_yr %>%
    with({
        pdf(here("market_pe.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        years <- 2000:2030

        ratio <- cap / val
        ratio_w <- cap_w / val_w

        plot(year, ratio, type = "n",
             main = "Market P/E (Equal-Weighted)",
             xlab = "", ylab = "Price-to-Income",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 30, 2.5), v = years, col = "lightblue")
        lines(year, ratio, lwd = 2)

        plot(year, ratio_w, type = "n",
             main = "Market P/E (Capitalization-Weighted)",
             xlab = "", ylab = "Price-to-Income",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 30, 2.5), v = years, col = "lightblue")
        lines(year, ratio_w, lwd = 2)

        plot(year, ratio_w / ratio, type = "n",
             main = "P/E Weight Spread",
             xlab = "", ylab = "Cap-to-Equal Weight Spread",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 30, 0.1), v = years, col = "lightblue")
        lines(year, ratio_w / ratio, lwd = 2)

        par(op)
        dev.off()
    })

df_yr %>%
    with({
        pdf(here("market65eq.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        years <- 2000:2030
        eqt <- ast - lia
        ev <- cap + dbt - eqt
        ic <- dbt + csh

        ps <- cap / rev
        plot(year, ps, type = "n",
             main = "P/S of the 65",
             xlab = "", ylab = "Price-to-Sales",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 10, 0.5), v = years, col = "lightblue")
        lines(year, ps, lwd = 2)

        pe <- cap / inc
        plot(year, pe, type = "n",
             main = "P/E of the 65",
             xlab = "", ylab = "Price-to-Income",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 30, 2.5), v = years, col = "lightblue")
        lines(year, pe, lwd = 2)

        pb <- cap / eqt
        plot(year, pb, type = "n",
             main = "P/B of the 65",
             xlab = "", ylab = "Price-to-Equity",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 20, 0.5), v = years, col = "lightblue")
        lines(year, pb, lwd = 2)

        pev <- cap / ev
        plot(year, pev, type = "n",
             main = "P/EV of the 65",
             xlab = "", ylab = "Price-to-Enterprise Value",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 2, 0.05), v = years, col = "lightblue")
        lines(year, pev, lwd = 2)

        al <- ast / lia
        plot(year, al, type = "n",
             main = "A/L of the 65",
             xlab = "", ylab = "Assets-to-Liabilities",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 3, 0.1), v = years, col = "lightblue")
        lines(year, al, lwd = 2)

        mr <- inc / rev
        plot(year, mr, type = "n",
             main = "Profit Margin of the 65",
             xlab = "", ylab = "Income-to-Revenue",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 1, 0.01), v = years, col = "lightblue")
        lines(year, mr, lwd = 2)

        de <- dbt / eqt
        plot(year, de, type = "n",
             main = "D/B of the 65",
             xlab = "", ylab = "Debt-to-Equity",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 2, 0.1), v = years, col = "lightblue")
        lines(year, de, lwd = 2)

        roic <- inc / ic
        plot(year, roic, type = "n",
             main = "ROIC of the 65",
             xlab = "", ylab = "Income-to-Invested Capital",
             xaxt = "n")
        axis(1, years, las = 2)
        abline(h = seq(0, 2, 0.025), v = years, col = "lightblue")
        lines(year, roic, lwd = 2)

        par(op)
        dev.off()
    })


# VALUATION BY CAPITALIZATION


df_filt <- df %>%
    filter(year == 2022) %>%
    na.omit()

df_filt %>%
    with({
        pdf(here("market_val.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        ev <- cap + dbt - csh
        eqt <- ast - lia
        ult <- list(cap / rev,
                    cap / inc,
                    cap / inc,
                    cap / csh,
                    lia / rev,
                    lia / inc,
                    lia / inc,
                    lia / csh,
                    dbt / rev,
                    dbt / inc,
                    dbt / inc,
                    dbt / csh) %>%
            lapply(function(x) {
                x <- log(x)
                x <- x - mean(x)
                x / sd(x)
            }) %>%
            do.call(cbind, .) %>%
            rowMeans(na.rm = TRUE)

        plot(cap, ult, pch = 4, log = "x")

        print(data.frame(ticker, ult) %>% arrange(ult))

        par(op)
        dev.off()
    })
