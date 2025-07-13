# Coffee Brew Analyzer ☕

Tämä Haskell-projekti simuloi 30 päivän ajan päivittäisiä kahvin keittoaikoja. Datalle suoritetaan puhdistus, analyysi ja terminaalivisualisointi histogrammina.

## 🔧 Toiminnot

- **Simulointi**: Satunnaiset keittoajat (1.5–9.5 min), pyöristetty kymmenesosiin.
- **Puhdistus**: Poistaa keittoajat < 1.0 min tai > 15.0 min
- **Analyysi**: Keskiarvo, minimi, maksimi
- **Visualisointi**: ASCII-histogrammi terminaalissa

## 🚀 Käyttö

```bash
runhaskell Main.hs
