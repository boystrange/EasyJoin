@Protocol("*acquire | *release | (BUSY + FREE)")
public class Semaphore {
    @Operation public void acquire();
    @Operation public void release();
    @State private void BUSY();
    @State private void FREE(int n);
    @Reaction private void when_FREE_acquire(int n) {
	if (n > 1) this.FREE(n - 1);
	else this.BUSY();
    }
    @Reaction private void when_FREE_release(int n) {
	this.FREE(n + 1);
    }
    @Reaction private void when_BUSY_release() {
	this.FREE(1);
    }
    public Semaphore(int n) {
	assert n > 0;
	this.FREE(n);
    }
}
