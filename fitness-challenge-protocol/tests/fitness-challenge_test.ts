import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Can create a fitness challenge",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('fitness-challenge', 'create-challenge', [
                types.ascii("30-Day Running Challenge"),
                types.ascii("Run at least 5km daily for 30 days"),
                types.uint(1000000), // 1 STX stake
                types.uint(4320), // 30 days in blocks
                types.uint(1440), // 10 days verification period
                types.uint(100), // max 100 participants
                types.uint(8000) // 80% to winners
            ], deployer.address)
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
    }
});

Clarinet.test({
    name: "Can join a challenge",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const participant = accounts.get('wallet_1')!;
        
        // Create challenge first
        let block = chain.mineBlock([
            Tx.contractCall('fitness-challenge', 'create-challenge', [
                types.ascii("Test Challenge"),
                types.ascii("Test Description"),
                types.uint(1000000),
                types.uint(4320),
                types.uint(1440),
                types.uint(10),
                types.uint(8000)
            ], deployer.address)
        ]);
        
        // Join challenge
        block = chain.mineBlock([
            Tx.contractCall('fitness-challenge', 'join-challenge', [
                types.uint(1)
            ], participant.address)
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
    }
});