# P2P Bitcoin Escrow for Freelancers 
A decentralized escrow system for secure freelance payments using Stacks blockchain.

## 🌟 Features

- Create escrow agreements between clients and freelancers
- Secure fund holding in smart contract
- Two-party approval system
- Automated payment release
- Full transaction transparency

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks wallet
- STX tokens for transactions

### 📋 Contract Functions

1. `create-escrow`: Client creates new escrow with specified amount
2. `approve-work`: Client approves completed work
3. `approve-completion`: Freelancer confirms work completion
4. `release-payment`: Releases funds after both parties approve

### 💡 Usage Example

1. Client creates escrow:
```clarity
(contract-call? .p2p-escrow create-escrow 'FREELANCER_ADDRESS u1000)
```

2. Client approves work:
```clarity
(contract-call? .p2p-escrow approve-work u1)
```

3. Freelancer approves completion:
```clarity
(contract-call? .p2p-escrow approve-completion u1)
```

4. Payment releases automatically after both approvals

## 🔐 Security

- Funds locked in contract until both parties approve
- Only authorized parties can approve their respective parts
- Built-in error handling and validation

## 📝 License

MIT
```
