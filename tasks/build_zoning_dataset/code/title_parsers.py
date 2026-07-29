import re

RECLASS_RE = re.compile(
    r"Zoning Reclassification Map No\.\s*([0-9]+-[A-Z])\s+at\s+(.+?)\s*-\s*App No\.\s*([A-Z]?\d+)",
    re.IGNORECASE,
)
PD_RE = re.compile(r"Planned Development(?:\s+No\.)?\s*([0-9]+)", re.IGNORECASE)


def _clean(value: str | None) -> str | None:
    if not value:
        return None
    out = re.sub(r"\s+", " ", value).strip(" -")
    return out or None


def parse_title_fields(title: str | None) -> dict[str, str | None]:
    text = title or ""

    map_grid = None
    address_raw = None
    app_number = None
    pd_number = None

    match = RECLASS_RE.search(text)
    if match:
        map_grid = _clean(match.group(1))
        address_raw = _clean(match.group(2))
        app_number = _clean(match.group(3))

    pd_match = PD_RE.search(text)
    if pd_match:
        pd_number = _clean(pd_match.group(1))

    return {
        "map_grid": map_grid,
        "address_raw": address_raw,
        "app_number": app_number,
        "pd_number": pd_number,
    }
