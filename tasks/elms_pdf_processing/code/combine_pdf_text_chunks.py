import argparse
import csv
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("output_date_tag")
    parser.add_argument("chunk_date_tags", nargs="+")
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    output_path = Path(f"../output/pdf_text_{args.output_date_tag}.csv")
    writer = None
    output_handle = output_path.open("w", newline="")
    try:
        for date_tag in args.chunk_date_tags:
            input_path = Path(f"../output/pdf_text_{date_tag}.csv")
            with input_path.open(newline="") as input_handle:
                reader = csv.DictReader(input_handle)
                if reader.fieldnames is None:
                    raise ValueError(f"Missing header in {input_path}")
                if writer is None:
                    writer = csv.DictWriter(output_handle, fieldnames=reader.fieldnames)
                    writer.writeheader()
                elif reader.fieldnames != writer.fieldnames:
                    raise ValueError(f"Unexpected columns in {input_path}")
                for row in reader:
                    writer.writerow(row)
    finally:
        output_handle.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
