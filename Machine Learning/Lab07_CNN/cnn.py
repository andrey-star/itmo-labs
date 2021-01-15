import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torchvision import datasets, transforms
from torch.optim.lr_scheduler import StepLR

from sklearn.metrics import confusion_matrix
from sklearn.metrics import ConfusionMatrixDisplay
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt


class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        self.conv1 = nn.Conv2d(1, 32, 3, 1)
        self.conv2 = nn.Conv2d(32, 64, 3, 1)
        self.dropout1 = nn.Dropout(0.25)
        self.fc1 = nn.Linear(9216, 128)
        self.dropout2 = nn.Dropout(0.5)
        self.fc2 = nn.Linear(128, 10)

    def forward(self, x):
        x = self.conv1(x)
        x = F.relu(x)
        x = self.conv2(x)
        x = F.relu(x)
        x = F.max_pool2d(x, 2)
        x = self.dropout1(x)
        x = torch.flatten(x, 1)
        x = self.fc1(x)
        x = F.relu(x)
        x = self.dropout2(x)
        x = self.fc2(x)
        output = F.log_softmax(x, dim=1)
        return output


def train(model, device, train_loader, optimizer, epoch):
    model.train()
    for batch, (data, target) in enumerate(train_loader):
        data, target = data.to(device), target.to(device)
        optimizer.zero_grad()
        output = model(data)
        loss = F.cross_entropy(output, target)
        loss.backward()
        optimizer.step()
        if batch % 10 == 0:
            print('epoch: {}, loss: {:.6f}, {:.0f}%'.format(
                epoch, loss.item(), 100. * batch / len(train_loader)))


def test(model, device, test_loader):
    model.eval()
    correct = 0
    for batch, (data, target) in enumerate(test_loader):
        data, target = data.to(device), target.to(device)
        output = model(data)
        pred = output.argmax(dim=1, keepdim=True)
        correct += pred.eq(target.view_as(pred)).sum().item()

    print('Accuracy: {:.0f}%\n'.format(100. * correct / len(test_loader.dataset)))


def run_train(train_dataset, test_dataset, epochs, file=""):
    torch.manual_seed(4)
    device = torch.device("cuda")

    train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=50, shuffle=True,
                                               num_workers=1, pin_memory=True)
    test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=1000, shuffle=True,
                                              num_workers=1, pin_memory=True)

    model = Net().to(device)
    optimizer = optim.Adam(model.parameters(), lr=1e-4)

    scheduler = StepLR(optimizer, step_size=1)
    for epoch in range(1, epochs + 1):
        train(model, device, train_loader, optimizer, epoch)
        test(model, device, test_loader)
        scheduler.step(epoch)
    if file != "":
        torch.save(model.state_dict(), file)


def plot_cm(y_true, y_pred):
    cf = confusion_matrix(y_true.view_as(y_pred), y_pred)
    ConfusionMatrixDisplay(cf).plot()
    plt.title('Accuracy: {:.0f}%'.format(100. * accuracy_score(y_true, y_pred)))
    plt.show()


def plot_digits(best_data):
    fig = plt.figure(figsize=(28, 28))
    columns = 10
    rows = 10
    for i in range(0, rows):
        for j in range(0, columns):
            img = best_data[i][j]
            fig.add_subplot(rows, columns, i * columns + j + 1)
            plt.imshow(img, cmap='gray')
    plt.show()


def build_plots(file, test_dataset):
    device = torch.device("cuda")
    model = Net().to(device)
    model.load_state_dict(torch.load(file))
    test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=1000, shuffle=True,
                                              num_workers=1, pin_memory=True)

    all_preds = None
    all_trues = None
    best_score = np.full(shape=(len(test_dataset), len(test_dataset)), fill_value=-1e20)
    best_data = []
    for i in range(0, len(test_dataset)):
        best_data.append([None] * len(test_dataset))

    model.eval()
    with torch.no_grad():
        for batch, (data_cpu, target_cpu) in enumerate(test_loader):
            data, target = data_cpu.to(device), target_cpu.to(device)
            output = model(data)
            for x, preds in enumerate(output):
                for y_pred, conf in enumerate(preds):
                    score = conf.item()
                    y_true = target_cpu.numpy()[x]
                    if best_score[y_true][y_pred] < score:
                        best_score[y_true][y_pred] = score
                        best_data[y_true][y_pred] = data_cpu.numpy()[x][0]
            y_pred = output.argmax(dim=1, keepdim=True)
            y_true = target.view_as(y_pred)
            if all_preds is None:
                all_preds = y_pred.flatten()
                all_trues = y_true.flatten()
            else:
                all_preds = torch.cat((all_preds, y_pred.flatten()))
                all_trues = torch.cat((all_trues, y_true.flatten()))
        plot_cm(all_trues.cpu(), all_preds.cpu())
        plot_digits(best_data)


if __name__ == '__main__':
    train_dataset = datasets.MNIST('data', train=True, transform=transforms.ToTensor())
    test_dataset = datasets.MNIST('data', train=False, transform=transforms.ToTensor())
    file = "log_sm,lr=1e-4.pt"
    # run_train(train_dataset, test_dataset, epochs=10, file=file)
    build_plots(file, test_dataset)
