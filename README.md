# VVN R Dashboard - source code

A copy of the two files that define each dashboard, one folder per project, so
the whole suite can be read from one place.

| Folder | Live dashboard |
|---|---|
| `Arts_Vibrancy_Maps/` | https://01a156e3-f19e-cec5-4bae-2f6d434d9e4d.share.connect.posit.cloud/ |
| `Childcare_Maps/` | https://01a14d78-dfa8-95af-e836-0e06d7649859.share.connect.posit.cloud/ |
| `Cost_of_Living_Maps/` | https://01a14d79-28bf-101e-307d-1c30a40fd3dc.share.connect.posit.cloud/ |
| `Crop_Maps/` | https://01a14d78-b04c-7004-4ae5-93b4b9fee1ac.share.connect.posit.cloud/ |
| `Dairy_Maps/` | https://01a14d7c-cf88-4db7-6b14-30ea0d937de6.share.connect.posit.cloud/ |
| `Data_Centers_Maps/` | https://01a156e3-b00c-e65f-1d75-627cd7193a73.share.connect.posit.cloud/ |
| `Rural_Health_Maps/` | https://01a1518c-a3c0-db66-9996-06250d00d13d.share.connect.posit.cloud/ |
| `Solar_Maps/` | https://01a14d76-a411-40be-11f4-58799b237c5a.share.connect.posit.cloud/ |
| `VCE_Maps/` | https://01a14d78-468d-db11-2e98-2951d33595ac.share.connect.posit.cloud/ |
| `VCE_AI_Maps/` | retired — the deployment was deleted; the code is kept for reference |

## The two files

- **`<App>_SMaps.R`** — the dashboard. Loads the data, builds the maps and
  charts, defines the UI and the server. Everything that decides what a reader
  sees is in this one file.
- **`app.R`** — a two-line entrypoint. Posit Connect Cloud requires a file
  called `app.R`, so it sources the script above and returns the app object.

## How to read a dashboard script

They are all laid out in the order the app starts:

1. **Libraries and constants.** Including the flags that control how a figure is
   calculated. Where a dashboard deliberately reproduces, or departs from, a
   published figure, that is a named constant with the reasoning in a comment
   above it — `USE_ORIGINAL_TOTALS` in `VCE_Maps` is the clearest example.
   Those comments are the place to start.
2. **Loaders.** Reading each app's own `*_Data/` folder, usually memoised so a
   slow fetch happens once per process rather than once per page view.
3. **Map and chart builders.** Palettes, break points, labelling.
4. **UI.** Tabs, sidebars, cards.
5. **Server.** One reactive per output, named after the output it feeds.

Shared helpers are in `vvn_ui.R`, which sits in every app directory and is the
same file in each: the theme, the colour ramps (one hue per dashboard), the
Virginia locality matching, and the map basemap and outline.

## This is a read-only mirror

These files are copies, published for reading. They are not what runs, and they
cannot be run from this repository on their own: each dashboard also needs its
own `*_Data/` folder and the shared `vvn_ui.R`, which live alongside it in the
main repository.

The working copies, the data and the deployment tooling are in
[VVN-R-Dashboard](https://github.com/VVN-VT/VVN-R-Dashboard), refreshed with
`./tools/sync-source-code.sh`. If a dashboard has changed recently, that
repository is the authority and this one may lag.
