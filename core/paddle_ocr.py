import sys

if __name__ == "__main__":
    image_path = sys.argv[1]
    from paddleocr import PaddleOCR
    ocr = PaddleOCR()
    result = ocr.ocr(image_path)
    string = ''.join(list(map(lambda r: r[1][0], result[0]))).replace(" ,", ",")
    print(string, flush=True)
