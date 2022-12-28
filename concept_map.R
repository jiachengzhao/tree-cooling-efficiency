## ----------------------------------
# Title: Concept map of tree cooling efficiency (TCE)
# Objective: Concept map illustrating how TCE changes with tree canopy cover (TCC) and air temperature (Ta)
# Created by: Jiacheng Zhao
# Created on: 2021-12-26
# Copyright (c) Jiacheng Zhao, 2021
# Beijing Normal University
# Email: zhaojiacheng@mail.bnu.edu.cn
## ----------------------------------

library(data.table)
opar = par(no.readonly = T)

# settings ----
# par
par(
  # family = 'Calibri',
  cex.axis = 0.9,
  las = 1,
  lwd = 0.1,
  mai = c(0, 0.4, 0, 0.4),
  mfrow = c(1, 2),
  oma = c(2, 2, 2, 0),
  pty = 's',
  tck = 0.02
)
cex.font = 0.9; cex.legend = 0.8
lwd = 2.5; lwd.axis = 0.7
seg.len = 1.5
cols = c('blueviolet', 'green1', 'red1')
col.slope = 'black'
x.intersp = 0.9; y.intersp = 0.7
inset = c(0, 0)
label.line = 1.3


# plot 1 ----
plot(
  0, type = 'n',
  xaxs = 'i', yaxs = 'i',
  xlab = '', ylab = '',
  xlim = c(0, 50), ylim = c(10, 40),
  axes = F
)
# x range
x = seq(2, 45, 0.1)
# parameter a
a1 = 18; a2 = 30; a3 = 45
a = c(a1, a2, a3)
# parameter b
b1 = -0.13; b2 = -0.18; b3 = -0.23
b = c(b1, b2, b3)
# lines
for (i in 1:3) {
  lines(y ~ x, data = data.table(x = x, y = a[i] * x ^ b[i]), col = cols[i], lwd = lwd)
}
# TCC location
location = c(10, 20)
# slope line
extension = 3.5
for (i in 1:2) {
  for (j in 1:3) {
    y = a[j] * location[i] ^ b[j] - 0.1
    k = a[j] * b[j] * location[i] ^ (b[j] - 1) 
    intercept = y - k * location[i] 
    segments(location[i] - extension, k * (location[i] - extension) + intercept, location[i] + extension, k * (location[i] + extension) + intercept, col = col.slope, lty = 'solid', lwd = 2)
  }
}
# axis
axis(1, at = c(-10, seq(0, 50, 10)), lwd = lwd.axis, mgp = c(3, 0.1, 0))
axis(2, at = c(-10, seq(0, 50, 10)), lwd = lwd.axis, mgp = c(3, 0.2, 0))
axis(3, at = c(-10, seq(0, 50, 10)), labels = F, lwd = lwd.axis, mgp = c(3, 0.1, 0))
axis(4, at = c(-10, seq(0, 50, 10)), labels = F, lwd = lwd.axis, mgp = c(3, 0.2, 0))
# surface cooling benefits
segments(10, a3 * 10 ^ b3, 10, 0, lty = 'dashed', lwd = 0.8)
segments(20, a3 * 20 ^ b3, 20, 0, lty = 'dashed', lwd = 0.8)
segments(0, a3 * 10 ^ b3, 22, a3 * 10 ^ b3, lty = 'dashed', lwd = 0.8)
segments(0, a3 * 20 ^ b3, 22, a3 * 20 ^ b3, lty = 'dashed', lwd = 0.8)
arrows(20, a[3] * 20 ^ b[3], 20, a[3] * 10 ^ b[3], length = 0.05, angle = 25, code = 3, lwd = 0.5)
arrows(10, a[3] * 20 ^ b[3], 20, a[3] * 20 ^ b[3], length = 0.05, angle = 25, code = 3, lwd = 0.5)
# annotation
text(20, 28.5, labels = expression('TCE = -' * Delta * 'LST/' * Delta * 'TCC'), cex = cex.legend)
text(16, 20.5, labels = 'TCC changes', cex = cex.legend)
text(30, 24.4, labels = 'Cooling benefits', cex = cex.legend)
# axis label
mtext(1, text = 'TCC (%)', line = label.line, cex = cex.font + 0.1)
mtext(2, text = expression('LST (' * degree * 'C)'), line = label.line + 0.2, cex = cex.font + 0.1, las = 0)
# legend
legend(
  'topright',
  legend = c(
    expression('Low T'[a]),
    expression('Medium T'[a]),
    expression('High T'[a]),
    'Slope line'
  ),
  lty = rep('solid', 4),
  seg.len = seg.len,
  lwd = 1.2,
  col = c(cols, col.slope),
  cex = cex.legend, bty = 'n',
  x.intersp = x.intersp, y.intersp = y.intersp,
  inset = c(-0.1, 0)
)

# plot 2 ----
# jplot(xlim = c(0, 40), ylim = c(0, 0.45), at2 = seq(0, 0.5, 0.1))
plot(
  0, type = 'n',
  xaxs = 'i', yaxs = 'i',
  xlab = '', ylab = '',
  xlim = c(0, 40), ylim = c(0, 0.4),
  axes = F
)
# intercept
intcp1 = 0.05; intcp2 = 0.02; intcp3 = 0.01
intcp = c(intcp1, intcp2, intcp3)
# slope
s1 = 0.01; s2 = 0.007; s3 = 0.004
s = c(s1, s2, s3)
# x range
x0 = 5; x1 = 33
# lines
for (i in 1:3) {
  y0 = intcp[i] + s[i] * x0
  y1 = intcp[i] + s[i] * x1
  segments(x0, y0, x1, y1, col = rev(cols)[i], lwd = lwd)
}
# axis
axis(1, at = c(-10, seq(0, 40, 10)), lwd = lwd.axis, mgp = c(3, 0.1, 0))
axis(2, at = c(-10, seq(0, 0.45, 0.1)), lwd = lwd.axis, mgp = c(3, 0.2, 0))
axis(3, at = c(-10, seq(0, 40, 10)), labels = F, lwd = lwd.axis, mgp = c(3, 0.1, 0))
axis(4, at = c(-10, seq(0, 0.45, 0.1)), labels = F, lwd = lwd.axis, mgp = c(3, 0.2, 0))
# axis label
mtext(1, text = expression('T'[a] * ' (' * degree * 'C)'), line = label.line, cex = cex.font)
mtext(2, text = expression('TCE (' * degree * 'C/%)'), line = label.line + 0.2, cex = cex.font, las = 0)
# legend
legend(
  'topleft',
  legend = c(
    'Low TCC',
    'Medium TCC',
    'High TCC'
  ),
  lty = rep('solid', 3),
  seg.len = seg.len,
  lwd = 1.2,
  col = rev(cols),
  cex = cex.legend, bty = 'n',
  x.intersp = x.intersp, y.intersp = y.intersp,
  inset = inset
)
par(opar)