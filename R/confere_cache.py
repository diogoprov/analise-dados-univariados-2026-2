import io, re, sys

PAD = 'file_refit'

def norm(t):
    t = re.sub(r"\s+", " ", t).strip()
    t = t.replace('"../cache/', '"cache/')
    t = re.sub(r",\s*" + PAD + r"\s*=\s*[^,)]+", "", t)
    return t

def brms(p):
    s = io.open(p, encoding="utf-8").read()
    out = []
    for m in re.finditer(r"brm\(", s):
        i = m.start(); n = 0; j = i + 3
        while j < len(s):
            if s[j] == "(":
                n += 1
            elif s[j] == ")":
                n -= 1
                if n == 0:
                    break
            j += 1
        out.append(s[i:j + 1])
    return out

sc = {norm(c) for c in brms("R/precompila-cache.R")}
pat = re.compile(r'file = "([^"]+)"')
ok = True
for f in ["slides/e02.qmd", "slides/e08.qmd", "slides/e09.qmd"]:
    for c in brms(f):
        if "file =" in c and "..." not in c:
            bate = norm(c) in sc
            ok = ok and bate
            print("  %-8s %-16s %s" % ("OK" if bate else "DIVERGE", f, pat.search(c).group(1)))
            if not bate:
                print("      slide :", norm(c))
print()
print("todas as chamadas batem" if ok else "!!! DIVERGENCIA -- corrigir antes de rodar")
sys.exit(0 if ok else 1)
