# NFT Receipts for Physical Goods
A Clarity smart contract system that enables retailers to issue digital proof-of-purchase NFTs for warranties, resale verification, and authentic ownership tracking of physical goods.

## 🌟 Features

- 🏪 **Retailer Authorization**: Only verified retailers can issue receipt NFTs
- 🧾 **Digital Receipts**: Immutable proof of purchase with warranty tracking
- 🔄 **Transferable Ownership**: Support for resale with ownership history
- ⚖️ **Warranty Claims**: Built-in warranty claim filing and resolution system
- 🛡️ **Authenticity Verification**: Verify receipt authenticity and retailer reputation
- 📊 **Retailer Profiles**: Track retailer reputation and issuance history

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Node.js for testing

### Installation

```bash
git clone <repository-url>
cd nft-receipts-project
```

### Running Tests

```bash
clarinet test
```

### Deploying Contract

```bash
clarinet deploy
```

## 🔧 Contract Functions

### Core Functions
- `authorize-retailer` - Authorize retailers to issue receipts
- `issue-receipt` - Create new receipt NFT for customer
- `transfer-receipt` - Transfer receipt to new owner
- `file-warranty-claim` - File warranty claim
- `resolve-warranty-claim` - Resolve warranty claims

### Read-Only Functions
- `get-receipt-data` - Get receipt details
- `get-warranty-status` - Check warranty validity
- `validate-receipt-authenticity` - Verify receipt authenticity
- `get-retailer-profile` - Get retailer information

## 🛡️ Security Features

- ✅ **Access Control**: Role-based permissions for retailers and contract owner
- ✅ **Input Validation**: Comprehensive validation of all inputs
- ✅ **Assert Guards**: Proper use of asserts for security checks
- ✅ **Transfer Restrictions**: Configurable transfer permissions per receipt
- ✅ **Warranty Expiration**: Automatic warranty period validation

## ⚡ Optimizations

- 🔄 **Efficient Data Storage**: Optimized map structures for gas efficiency
- 📊 **Batch Operations**: Support for multiple operations in single transaction
- 🎯 **Minimal State Changes**: Reduced unnecessary state modifications
- 📈 **Reputation Tracking**: Automated retailer reputation management

## 🧪 Test Coverage

- ✅ Authorization and permission testing
- ✅ Receipt issuance and transfer scenarios
- ✅ Warranty claim lifecycle testing
- ✅ Edge cases and error conditions
- ✅ Security validation tests

## 🖼️ Suggested UI Features

### 📱 Customer Dashboard
- View owned receipts with warranty status
- Transfer receipts to new owners
- File warranty claims with status tracking
- Verify receipt authenticity

### 🏪 Retailer Portal
- Issue new receipts for customers
- Manage warranty claims and resolutions
- View reputation score and statistics
- Bulk receipt operations

### 👑 Admin Panel
- Authorize/revoke retailer permissions
- Monitor system-wide statistics
- Manage retailer reputation scores
- System configuration settings

## 🎯 Use Cases

- 🛒 **E-commerce**: Digital receipts for online purchases
- 🏬 **Retail Stores**: In-store purchase verification
- 🔧 **Warranty Management**: Streamlined warranty claim process
- 💰 **Resale Markets**: Authentic ownership verification
- 📱 **Electronics**: High-value item tracking and warranty

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Add comprehensive tests
4. Submit pull request

## 📄 License

MIT License - see LICENSE file for details

---

*Built with ❤️ using Clarity and Stacks blockchain*
```
