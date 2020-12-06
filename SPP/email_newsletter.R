library(keyring)
library('rmarkdown')
library("blastula")
library("glue")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helperMethods.R")
library(ggplot2)
library(formattable)


df <- getForecastVsActual()


p1 <- df %>%
  filter(variable %in% c("wind_pen_actual", "wind_pen_STF", "wind_pen_MTF")) %>%
  ggplot(aes(
    x = Interval,
    y = value,
    col = variable,
    text = paste(
      "</br>Date: ",
      Interval,
      "</br>Value: ",
      value,
      "</br>Category: ",
      variable
    )
  )) + geom_point() + geom_line() + theme_minimal() + labs(y = "Wind Penetration (%)", x = "Datetime", col = "Category")

plot_image <- ggplotly(p1, tooltip = c("text"))



date_time <- add_readable_time()
plot_email <- add_ggplot(p1, height = 12.266666666666667, width = 14.4)


# Create an HTML table with `format_table()`
formatted_table <-
  format_table(
    x = head(df)
    )

email <- compose_email(
  body = md(
    c("Wind Penetration Statistics", plot_email)),
  footer = md(c("Email sent on ", date_time,"."))
)


Sys.setenv(SMTP_PASSWORD="Ooct248014")


email %>%
  smtp_send(
    from = "sppim.newsletter@gmail.com",
    to = "dwarakvaradharajan@gmail.com",
    subject = "Testing the `smtp_send()` function",
    credentials = creds_envvar(
      user = "sppim.newsletter@gmail.com",
      pass_envvar = "SMTP_PASSWORD",
      provider = "gmail",
      host = NULL,
      port = NULL,
      use_ssl = TRUE
    )
    )
