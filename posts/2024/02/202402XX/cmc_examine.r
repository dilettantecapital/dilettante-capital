source("lib/library.r")


# PREAMBLE


# - Debt - Cash = Net Debt
#   - The difference between total debt and cash.
# - Debt / Equity Ratio
#   - A company's financial leverage, indicating its reliance on debt
#     financing.
# - Debt / Total Assets
#   - The proportion of a company's assets financed by debt.
# - Market Capitalization / Cash
#   - Compares a company's market capitalization to its cash holdings. Provides
#     insights into how the market values the company's cash resources.
# - Market Capitalization / Debt
#   - The market capitalization compared to the company's debt level.
# - Market Capitalization / Net Income (P/E ratio)
#   - How much investors are willing to pay for each dollar of a company's
#     profit.
# - Market Capitalization / Revenue (P/S ratio)
#   - How much the market values each dollar of a company's sales.
# - Market Capitalization / Total Assets
#   - How much the market values each dollar of a company's assets.
# - Market Capitalization / Total Liabilities
#   - Compares a company's market capitalization to its total liabilities.
#     Provides insights into the market's assessment of the company's debt
#     obligations.
# - Market Capitalization + Debt - Cash = Enterprise Value
# - Net Income / Debt (Interest Coverage Ratio)
#   - A company's ability to service its debt with its earnings.
# - Net Income / Revenue (Profit Margin)
#   - The percentage of each dollar of revenue that a company keeps as profit.
# - Net Income / Total Assets (Return on Assets - ROA)
#   - The profitability generated from each dollar of a company's assets.
# - Revenue / Total Assets
#   - How efficiently a company generates revenue using its assets.
# - Total Assets - Total Liabilities = Shareholders' Equity
#   - Fundamental accounting equation.
# - Total Liabilities - Debt = Other Liabilities
#   - e.g., accounts payable, accrued expenses.


# SETUP


df <- here("cmc_full.csv") %>%
    read_csv()


# COMBOS


vec <- c("Market Capitalization", "Revenue", "Net Income", "Total Assets", "Total Liabilites", "Debt", "Cash")
for (i in vec) {
    for (j in vec) {
        cat(i, " / ", j, " = ???", "\n", sep = "")
    }
}


# EXAMINATION


zero_na <- function(x) {
    ifelse(x <= 0, NA, x)
}

df_rat <- df %>%
    fill(rev, inc, ast, lia, dbt, csh, .direction = "up") %>%
    filter(year == 2024) %>%
    mutate(inc = zero_na(inc),
           eqt = zero_na(ast - lia),
           ev = cap + dbt - csh,
           ic = dbt + csh,
           ps = cap / rev,
           pe = cap / inc,
           pb = cap / eqt,
           pc = cap / csh,
           pev = cap / ev,
           da = dbt / ast,
           de = dbt / eqt,
           mar = rev / inc,
           srv = dbt / inc,
           roa = ast / inc,
           roe = eqt / inc,
           roi = ic / inc) %>%
    select(-year, -cap, -rev, -inc, -ast, -lia, -dbt, -csh, -eqt, -ev, -ic)

mat <- df_rat %>%
    tibble::column_to_rownames("ticker")

rnk <- mat %>%
    apply(2, function(x) {
        out <- match(x, sort(x))
        out[is.na(out)] <- 100
        out
    })

data.frame(ticker = df_rat$ticker, rnk) %>%
    arrange(ticker) %>%
    as_tibble() %>%
    filter(ticker == "DHR")

data.frame(ticker = df_rat$ticker, rank = rowMeans(rnk)) %>%
    arrange(rank)


# ULTIMATE BALANCE


good <- c("cap", "lia", "dbt")
bad <- c("rev", "inc", "inc", "csh")
for (i in good) {
    for (j in bad) {
        cat("(", i, "/", j, ") + ", sep = "")
    }
}

df %>%
    fill(rev, inc, ast, lia, dbt, csh, .direction = "up") %>%
    filter(year == 2024) %>%
    mutate(ratio = log(cap / rev) + log(cap / inc) + log(cap / inc) + log(cap / csh) + log(lia / rev) + log(lia / inc) + log(lia / inc) + log(lia / csh) + log(dbt / rev) + log(dbt / inc) + log(dbt / inc) + log(dbt / csh)) %>%
    select(-year, -cap, -rev, -inc, -ast, -lia, -dbt, -csh) %>%
    arrange(ratio) %>%
    print(n = 100)
