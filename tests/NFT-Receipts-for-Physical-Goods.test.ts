import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
  name: "Can authorize retailer and issue receipt",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const retailer = accounts.get('wallet_1')!;
    const customer = accounts.get('wallet_2')!;

    let block = chain.mineBlock([
      Tx.contractCall('nft-receipts', 'authorize-retailer', [
        types.principal(retailer.address),
        types.ascii("Best Electronics Store")
      ], deployer.address)
    ]);
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), true);

    block = chain.mineBlock([
      Tx.contractCall('nft-receipts', 'issue-receipt', [
        types.principal(customer.address),
        types.ascii("iPhone 15"),
        types.ascii("Electronics"),
        types.uint(999),
        types.uint(8760),
        types.ascii("ABC123456"),
        types.bool(true),
        types.ascii("https://metadata.example.com/1")
      ], retailer.address)
    ]);
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
  },
});

Clarinet.test({
  name: "Unauthorized retailer cannot issue receipt",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const unauthorized = accounts.get('wallet_1')!;
    const customer = accounts.get('wallet_2')!;

    let block = chain.mineBlock([
      Tx.contractCall('nft-receipts', 'issue-receipt', [
        types.principal(customer.address),
        types.ascii("iPhone 15"),
        types.ascii("Electronics"),
        types.uint(999),
        types.uint(8760),
        types.ascii("ABC123456"),
        types.bool(true),
        types.ascii("https://metadata.example.com/1")
      ], unauthorized.address)
    ]);
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectErr(), types.uint(100));
  },
});

Clarinet.test({
  name: "Can transfer transferable receipt",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const retailer = accounts.get('wallet_1')!;
    const customer = accounts.get('wallet_2')!;
    const newOwner = accounts.get('wallet_3')!;

    chain.mineBlock([
      Tx.contractCall('nft-receipts', 'authorize-retailer', [
        types.principal(retailer.address),
        types.ascii("Best Electronics Store")
      ], deployer.address)
    ]);

    chain.mineBlock([
      Tx.contractCall('nft-receipts', 'issue-receipt', [
        types.principal(customer.address),
        types.ascii("iPhone 15"),
        types.ascii("Electronics"),
        types.uint(999),
        types.uint(8760),
        types.ascii("ABC123456"),
        types.bool(true),
        types.ascii("https://metadata.example.com/1")
      ], retailer.address)
    ]);

    let block = chain.mineBlock([
      Tx.contractCall('nft-receipts', 'transfer-receipt', [
        types.uint(1),
        types.principal(customer.address),
        types.principal(newOwner.address)
      ], customer.address)
    ]);
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), true);
  },
});

Clarinet.test({
  name: "Can file and resolve warranty claim",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const retailer = accounts.get('wallet_1')!;
    const customer = accounts.get('wallet_2')!;

    chain.mineBlock([
      Tx.contractCall('nft-receipts', 'authorize-retailer', [
        types.principal(retailer.address),
        types.ascii("Best Electronics Store")
      ], deployer.address)
    ]);

    chain.mineBlock([
      Tx.
