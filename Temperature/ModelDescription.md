---
output: html_document
---

# Population dynamics model
The model projects the dynamics of two life stages, juveniles and adults.
$$
\frac{dJ}{dt}= r[T]Ae^{-A}-m[T]J-d_J[T]J
$$
$$
\frac{dJ}{dt} = m[T]J-d_A[T]A
$$

$J$ and $A$ are the densities of the juvenile and adult stages, respectively, and $r[T]$, $m[T]$, $d_J[T]$, and $d_A[T]$ are the temperature responses of reproduction, development, juvenile mortality, and adult mortality, respectively.

# Vital rate temperature-response functions
$$
r[T] = r_{Topt}e^{-\frac{(T-T_{opt})^2}{2s^{2}}}
$$
$$
m[T] = m_R\frac{T}{T_R}\frac{e^{A_m(\frac{1}{T_R}-\frac{1}{T})}}{1+e^{A_L(\frac{1}{T_L}-\frac{1}{T})}+e^{A_H(\frac{1}{T_H}-\frac{1}{T})}}
$$
$$
d_J[T] = d_{JR}e^{A_{dJ}(\frac{1}{T_R}-\frac{1}{T})}
$$
$$
d_A[T] = d_{AR}e^{A_{dA}(\frac{1}{T_R}-\frac{1}{T})}
$$

# Parameters
| Parameter  |   | Value |
|------------|---|-------|
| $r_{Topt}$ |   | 1     |
| $T_{opt}$  |   | 293   |
| $s$        |   | 5     |
| $m_R$      |   | 0.1   |
| $T_R$      |   | 283   |
| $A_m$      |   | 15000 |
| $A_L$      |   | 5000  |
| $A_H$      |   | 70000 |
| $T_L$      |   | 278   |
| $T_H$      |   | 295   |
| $d_{JR}$   |   | 0.1   |
| $A_{dJ}$   |   | 5000  |
| $d_{AR}$   |   | 0.1   |
| $A_{dA}$   |   | 2500  |
