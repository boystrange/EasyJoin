@Protocol("*acquireRead · *acquireWrite · (FREE + UNIQUE · releaseWrite + SHARED · *releaseRead)")
public class ReadWriteLock {
    @State private void FREE();
    @State private void UNIQUE();
    @State private void SHARED(int readers);

    @Operation public void acquireRead();
    @Operation public void acquireWrite();
    @Operation public void releaseRead();
    @Operation public void releaseWrite();

    @Reaction private void when_FREE_acquireRead() { this.SHARED(1); }
    @Reaction private void when_FREE_acquireWrite() { this.UNIQUE(); }
    @Reaction private void when_SHARED_acquireRead(int readers) {
	this.SHARED(readers + 1);
    }
    @Reaction private void when_SHARED_releaseRead(int readers) {
	if (readers > 1) this.SHARED(readers - 1);
	else this.FREE();
    }
    @Reaction private void when_UNIQUE_releaseWrite() { this.FREE(); }

    public ReadWriteLock() { this.FREE(); }
}
