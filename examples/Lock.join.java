@Protocol("*acquire | (FREE + BUSY·release)")
public class Lock {
    @State private void FREE();
    @State private void BUSY();
    @Operation public void acquire();
    @Operation public void release();
    @Reaction private void when_FREE_acquire() { this.BUSY(); }
    @Reaction private void when_BUSY_release() { this.FREE(); }
    public Lock() { this.FREE(); }
}
