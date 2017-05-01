0. 프로그램 소개
  이 프로그램은 Cloud Haskell 테스트를 위해 작성된 프로그램입니다.
  master 노드에서 구하기를 원하는 피보나치 수를 입력하면 n까지의 피보나치 수를 구해줍니다.  

1. 사전에 필요한 프로그램
- GHC 7.10.3 이상
- STACK

2. 설치 방법
- stack build

3. 실행 방법
- master 노드 : stack exec fibo-exe master IP Port n
- slave 노드 : stack exec fibo-exe slave IP Port n