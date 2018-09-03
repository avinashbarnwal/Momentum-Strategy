# Quantitative Investing

Investment strategy is inspired by book Quantitative momentum [Quantitative Momentum](https://www.amazon.com/Quantitative-Momentum-Practitioners-Momentum-Based-Selection/dp/111923719X/ref=sr_1_1?ie=UTF8&qid=1478201274&sr=8-1&keywords=quantitative+momentum)

**Objective**

Portfolio Creation using momentum factor and P/B Factor. This also tests different size of stocks in portfolio to maximize portfolio returns. There is additional variant of momemtum factor. We call it **significant regression momentum portfolio**. Similar strategy is used for P/B factor.

**Structural Steps**

**Momentum Portfolio**

1. Stock population - S&P 500
2. Divide and choose , Large Cap and Mid Cap Stocks.
3. Rank the assets in Large Cap basis return in last 12 months(returns are monthly).
4. Equal Weight allocation among the specific size of portfolio.
5. Test the hypothesis across the size of portfolio basis per month returns and take the portfolio having maximum per month returns.
6. Similar steps for Mid Cap
7. Test the return for Mid Cap and Large Cap and invest accordingly.

**P/B Portfolio**


8. Rank the stocks based on P/B on each month and rebalance the portfolio based on equal weight.
9. Similar steps done in momentum portfolio.

**Regression Momentum Portfolio**

10. Rank the assets in Large Cap basis return in last 12 months(returns are monthly).
11. Test for linear regression between rank ad return for last 12 month.
12. Invest only in stocks where slope is significant.








