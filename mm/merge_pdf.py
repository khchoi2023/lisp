import sys
import re
from pathlib import Path

try:
    from pypdf import PdfWriter, PdfReader
except ImportError:
    print("오류: pypdf가 설치되어 있지 않습니다.")
    print("설치 명령: C:\\Python313\\python.exe -m pip install pypdf")
    sys.exit(1)


def natural_pdf_key(path: Path, base_name: str):
    """
    파일명 예:
      Tank01_001.pdf
      Tank01_002.pdf
    에서 뒤의 번호를 추출해 정렬 키로 사용
    """
    pattern = re.compile(rf"^{re.escape(base_name)}_(\d+)\.pdf$", re.IGNORECASE)
    m = pattern.match(path.name)
    if m:
        return int(m.group(1))
    return 10**9


def collect_target_pdfs(folder: Path, base_name: str):
    """
    지정 폴더에서 base_name_숫자.pdf 형식의 파일만 수집
    """
    pattern = re.compile(rf"^{re.escape(base_name)}_(\d+)\.pdf$", re.IGNORECASE)
    files = []

    for p in folder.iterdir():
        if p.is_file() and pattern.match(p.name):
            files.append(p)

    files.sort(key=lambda p: natural_pdf_key(p, base_name))
    return files


def merge_pdfs(folder: Path, base_name: str):
    """
    PDF 병합 수행
    """
    pdf_files = collect_target_pdfs(folder, base_name)

    if not pdf_files:
      print(f"병합 대상 PDF가 없습니다: {folder} / {base_name}_###.pdf")
      return 1

    output_file = folder / f"{base_name}_merged.pdf"

    writer = PdfWriter()

    print("병합 대상 파일:")
    for pdf in pdf_files:
        print(f" - {pdf.name}")
        reader = PdfReader(str(pdf))
        for page in reader.pages:
            writer.add_page(page)

    with open(output_file, "wb") as f:
        writer.write(f)

    print(f"\n병합 완료: {output_file}")
    return 0


def main():
    if len(sys.argv) < 3:
        print("사용법:")
        print('  python merge_pdf.py "폴더경로" "DWG이름"')
        print('예:')
        print(r'  python merge_pdf.py "E:\ProjectA" "Tank01"')
        sys.exit(1)

    folder = Path(sys.argv[1]).expanduser().resolve()
    base_name = sys.argv[2].strip()

    if not folder.exists():
        print(f"오류: 폴더가 존재하지 않습니다: {folder}")
        sys.exit(1)

    if not folder.is_dir():
        print(f"오류: 폴더가 아닙니다: {folder}")
        sys.exit(1)

    if not base_name:
        print("오류: DWG 이름이 비어 있습니다.")
        sys.exit(1)

    code = merge_pdfs(folder, base_name)
    sys.exit(code)


if __name__ == "__main__":
    main()